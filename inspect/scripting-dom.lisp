;;;; inspect/scripting-dom.lisp — a repeatable smoke test of the DOM/CSSOM/Events
;;;; and timer surface the weft/shuttle bridge exposes to <script>.
;;;;   sbcl --script inspect/scripting-dom.lisp
(require :asdf)
(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system "weft/script")

(defpackage #:weft.script.dom-test
  (:use #:cl) (:local-nicknames (#:s #:weft.script) (#:js #:shuttle) (#:h #:weft.html))
  (:export #:run))
(in-package #:weft.script.dom-test)

(defvar *pass* 0) (defvar *fail* 0)
(defun chk (label src expected)
  (let* ((doc (h:parse-html "<body><div id=x class=c>old</div></body>"))
         (ctx (s:make-context doc))
         (got (handler-case (js:to-string (js:eval-script (s:context-realm ctx) src))
                (error (e) (format nil "ERR ~a" e)))))
    (if (equal got expected) (incf *pass*)
        (progn (incf *fail*) (format t "~&FAIL ~a~%  got ~s want ~s~%" label got expected)))))

(defun run ()
  (setf *pass* 0 *fail* 0)
  ;; --- core node model ---
  (chk "nodeType-el" "document.getElementById('x').nodeType" "1")
  (chk "nodeName-el" "document.getElementById('x').nodeName" "DIV")
  (chk "tagName" "document.getElementById('x').tagName" "DIV")
  (chk "nodeType-text" "document.createTextNode('a').nodeType" "3")
  (chk "nodeType-doc" "document.nodeType" "9")
  (chk "nodeName-text" "document.createTextNode('a').nodeName" "#text")
  ;; --- traversal ---
  (chk "firstChild.data" "document.getElementById('x').firstChild.data" "old")
  (chk "parentNode" "document.getElementById('x').parentNode.nodeName" "BODY")
  (chk "childNodes.length" "document.body.childNodes.length" "1")
  (chk "ownerDocument" "String(document.getElementById('x').ownerDocument === document)" "true")
  ;; --- mutation ---
  (chk "createElement/append"
       "var p=document.createElement('p');p.appendChild(document.createTextNode('hi'));document.body.appendChild(p);document.body.lastChild.textContent"
       "hi")
  (chk "insertBefore"
       "var n=document.createElement('i');document.body.insertBefore(n,document.body.firstChild);document.body.firstChild.tagName"
       "I")
  (chk "removeChild"
       "var d=document.getElementById('x');d.parentNode.removeChild(d);String(document.getElementById('x'))"
       "null")
  (chk "replaceChild"
       "var n=document.createElement('b');document.body.replaceChild(n,document.getElementById('x'));document.body.firstChild.tagName"
       "B")
  (chk "cloneNode-deep"
       "var d=document.createElement('u');d.appendChild(document.createTextNode('z'));d.cloneNode(true).firstChild.data"
       "z")
  (chk "documentFragment"
       "var f=document.createDocumentFragment();f.appendChild(document.createElement('a'));f.appendChild(document.createElement('b'));document.body.appendChild(f);document.body.childNodes.length"
       "3")
  ;; --- attributes ---
  (chk "getAttribute" "document.getElementById('x').getAttribute('class')" "c")
  (chk "setAttribute" "var e=document.getElementById('x');e.setAttribute('data-y','1');e.getAttribute('data-y')" "1")
  (chk "hasAttribute" "String(document.getElementById('x').hasAttribute('id'))" "true")
  (chk "removeAttribute" "var e=document.getElementById('x');e.removeAttribute('class');String(e.hasAttribute('class'))" "false")
  (chk "id-reflect" "document.getElementById('x').id" "x")
  (chk "className-reflect" "document.getElementById('x').className" "c")
  ;; --- collections ---
  (chk "getElementsByTagName" "document.getElementsByTagName('div').length" "1")
  (chk "getElementsByClassName" "document.getElementsByClassName('c').length" "1")
  ;; --- events ---
  (chk "dispatch+detail"
       "var l='';var el=document.getElementById('x');el.addEventListener('t',function(e){l+=e.type+e.detail;},false);var ev=document.createEvent('Event');ev.initCustomEvent('t',true,false,9);el.dispatchEvent(ev);l"
       "t9")
  (chk "bubbling"
       "var l='';document.body.addEventListener('c',function(e){l+='B';});var ev=document.createEvent('Event');ev.initEvent('c',true,false);document.getElementById('x').dispatchEvent(ev);l"
       "B")
  (chk "preventDefault-return"
       "var el=document.getElementById('x');el.addEventListener('k',function(e){e.preventDefault();});var ev=document.createEvent('Event');ev.initEvent('k',false,true);String(el.dispatchEvent(ev))"
       "false")
  (chk "removeEventListener"
       "var c=0;var f=function(){c++;};var el=document.getElementById('x');el.addEventListener('z',f);el.removeEventListener('z',f);var ev=document.createEvent('Event');ev.initEvent('z',false,false);el.dispatchEvent(ev);String(c)"
       "0")
  (if (zerop *fail*)
      (format t "~&scripting DOM surface: ~a passed, 0 failed~%" *pass*)
      (format t "~&scripting DOM surface: ~a passed, ~a FAILED~%" *pass* *fail*))
  (values (zerop *fail*) *fail*))

(multiple-value-bind (ok) (run) (unless ok (uiop:quit 1)))
