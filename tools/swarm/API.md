# weft.script API digest — what a forms-*.lisp feature file can call

Prepended to swarm task files. Every signature below is real and current; you do
not need to grep for them. Anything NOT here, you do have to go look up.

## Registering the file
The last form in your file must be:

    (register-element-proto-extension :yourkey #'install-forms-yourname)

`install-element-proto` funcalls it as `(ctx ep)` after the built-in accessors,
so you are installing onto the SHARED element prototype — gate every accessor on
the tag name or you will break other elements.

Every feature file installs onto that same object and **whoever installs last
wins**. Registering a generic name (`checkValidity`, `willValidate`, `value`,
`labels`, `setCustomValidity`) without a tag gate deletes the sibling file's
working implementation, and returning `js:*undefined*` for the elements that
aren't yours is exactly as destructive as never defining it. Measured: one file
took its own unit 6 -> 36 while destroying 131 subtests across five other units.
**So use the `-for` variants for every name you did not invent.** They take a
tag name, run your body only for that tag, and delegate to whatever was
installed before for every other element:

    (defmethod-for ctx ep "select" "checkValidity" 0 (this args) ...)
    (defget-for    ctx ep "select" "type" (this) ...)
    (defgetset-for ctx ep "select" "value" (this) <getter> (v) <setter>)

Same bodies as the plain versions, one extra argument, and no off-tag branch to
write — drop the `(if (string= (h:dnode-name node) "select") ... js:*undefined*)`
wrapper entirely, the macro is the gate. Use the plain `defmethod*`/`defget`/
`defgetset` only for a name nothing else could plausibly define.

The oracle scores every unit, not just yours, and prints `TOTAL` — that is the
number that counts.

## Installing accessors and methods    [src/script/core.lisp:134-160]

    (defget     ctx ep "name" (this) body...)               ; read-only accessor
    (defgetset  ctx ep "name" (this) getter (v) setter)     ; v is the assigned JS value
    (defmethod* ctx ep "name" arity (this args) body...)    ; native method

Inside them:

    (require-node ctx this)   ; JS wrapper -> weft node; throws TypeError if not a node
    (arg args n)              ; nth argument, or js:*undefined* if absent
    (jstr v)                  ; JS value -> CL string   (js:to-string)
    (num x)                   ; CL real  -> JS number   (float x 1d0)
    (js:to-number v)          ; JS value -> double

`num` takes a CL REAL and nothing else. Handing it NIL, `js:*undefined*`, or any
other JS value signals a Lisp type error that escapes into the harness and kills
the ENTIRE REST OF THE TEST FILE, not just that subtest — it is the single most
expensive mistake available here. Convert first (`js:to-number`), and return
`js:*undefined*` / `js:*null*` directly rather than wrapping them.
    (wrap ctx node)           ; weft node -> JS wrapper

Return finished JS values: `js:*undefined*`, `js:*null*`, `js:*true*`,
`js:*false*`, a string, `(num n)`, or a wrapper. A getter that returns raw NIL
or wraps NIL in `num` aborts the whole test file at that subtest.

## Nodes and attributes                 [src/script/dom.lisp]

    (h:dnode-name node)          ; tag name, lowercase — this is your tag gate
    (h:dnode-kind node)          ; :element | :text | ...
    (h:dnode-children node)      ; a VECTOR (loop ... across), not a list
    (h:dnode-parent node)
    (h:dnode-data node)          ; text-node data
    (get-attr node "name")       ; string or NIL
    (set-attr node "name" "val") ; then (setf (context-dirty ctx) t) if it affects render
    (dom:has-attribute node "name")
    (child-text-content node)    ; concatenated DIRECT Text children only
    (tree-root node)
    (throw-dom ctx "IndexSizeError" 1 "message")

## Live collections                     [src/script/dom.lisp:1348]

    (make-collection ctx list-fn &optional name-fn kind)

`list-fn` is a thunk returning a FRESH CL LIST of weft nodes, re-read on every
access (that is what makes it live). `kind` is `:htmlcollection` (default) or
`:nodelist`; an :htmlcollection gets named access + `namedItem`, a :nodelist
does not. Pass `name-fn` only to override namedItem semantics.

## Conventions
- `(in-package #:weft.script)` at the top.
- Prefix every helper you define with your feature name — the files share one
  package, so an unprefixed `defun` will clobber a sibling's.
- Use `labels`, not `flet`, when your helpers call each other.

## A complete working file to copy the shape from
`src/script/forms-validity.lisp` — ~50 lines, uses defget/defmethod*/require-node
and is registered exactly as described above.
