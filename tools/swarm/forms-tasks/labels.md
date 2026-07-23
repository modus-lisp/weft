Implement the `labels` IDL attribute on `<input>` (and other labelable controls).

WPT file (READ it): /home/claude/wpt/html/semantics/forms/the-input-element/input-labels.html

`labels` returns a live NodeList of the `<label>` elements associated with the
control (empty for a non-labelable type: hidden).  A label is associated when:
  - its `for` attribute equals the control's `id` (and it is the control's tree),
    OR
  - the control is a descendant of the `<label>` (implicit association), and the
    label has no `for` (or its `for` targets this control).
Order: tree order. A label associated both ways counts once.

For a hidden input, `labels` returns null (js:*null*).

Build the NodeList with the in-tree helper:
  (make-collection ctx (lambda () (labels-for ctx node)) nil :nodelist)
where labels-for returns a LIST of label DOM nodes in document order. To walk the
document: from the node, go up to the document/root, then collect all "label"
elements. Use:
  (h:dnode-name X)        ; lowercased tag
  (h:dnode-children X)    ; vector of child nodes
  (h:dnode-parent X)      ; parent
  (get-attr LBL "for")    ; the label's for=  (string or NIL)
  (get-attr NODE "id")    ; the control's id
Find the document root by climbing dnode-parent until it is nil / the :document.

labels-for algorithm:
  id := (get-attr node "id")
  result := ()
  for each <label> L in the tree (document order):
     if L is an ancestor-of node (implicit) with no conflicting for -> include
     else if id is non-empty and (get-attr L "for") == id -> include
  return result (dedup, document order).

Only the FIRST labelable control for a given `for` matters, but for `labels` we
collect all labels pointing at THIS node, so just compare each label's `for` to
this node's id and check ancestry.

Prefix helpers `labels-`. Return js:*null* for hidden. This unit's oracle file
reports few subtests but currently 0 pass — getting association + ordering right
should flip it.
