;;;; src/css/packages.lisp — CSS package.
(defpackage #:weft.css
  (:use #:cl)
  (:export #:register-value-parser #:parse-value #:*value-parsers*
           #:define-value-parser
           ;; tokenizer + parser
           #:css-tokenize #:parse-stylesheet
           #:css-rule #:css-rule-selector #:css-rule-decls
           #:css-decl #:css-decl-prop #:css-decl-value #:css-decl-important
           #:ctok #:ctok-type #:ctok-value #:ctok-unit
           ;; selectors
           #:parse-selector-list #:selector-matches-p #:specificity
           #:query-select #:query-select-all
           ;; computed style
           #:compute-styles #:cstyle #:cstyle-display #:cstyle-color #:cstyle-background
           #:cstyle-font-size #:cstyle-font-weight #:cstyle-line-height
           #:cstyle-font-family #:cstyle-font-style
           #:cstyle-width #:cstyle-height #:cstyle-margin-top #:cstyle-margin-right
           #:cstyle-margin-bottom #:cstyle-margin-left #:cstyle-padding-top
           #:cstyle-padding-right #:cstyle-padding-bottom #:cstyle-padding-left
           #:cstyle-border-top-width #:cstyle-border-right-width #:cstyle-border-bottom-width
           #:cstyle-border-left-width #:cstyle-border-color #:cstyle-text-align #:cstyle-white-space
           #:cstyle-border-top-color #:cstyle-border-right-color #:cstyle-border-bottom-color #:cstyle-border-left-color
           #:cstyle-border-top-style #:cstyle-border-right-style #:cstyle-border-bottom-style #:cstyle-border-left-style
           #:border-edge-painted-p
           #:cstyle-text-decoration #:cstyle-list-style
           #:cstyle-max-width #:cstyle-min-width #:cstyle-margin-left-auto #:cstyle-margin-right-auto
           #:cstyle-float #:cstyle-clear #:cstyle-position #:cstyle-box-sizing #:cstyle-overflow
           #:cstyle-flex-direction #:cstyle-justify-content #:cstyle-align-items #:cstyle-align-content
           #:cstyle-flex-wrap #:cstyle-flex-grow #:cstyle-flex-shrink #:cstyle-flex-basis #:cstyle-order #:cstyle-gap
           #:cstyle-grid-template-columns #:cstyle-grid-template-rows #:cstyle-grid-auto-rows
           #:cstyle-grid-auto-flow #:cstyle-grid-column #:cstyle-grid-row
           #:cstyle-grid-area #:cstyle-grid-template-areas
           #:cstyle-row-gap #:cstyle-column-gap
           #:cstyle-justify-items #:cstyle-justify-self #:cstyle-align-self
           #:cstyle-top #:cstyle-left #:cstyle-right #:cstyle-bottom #:cstyle-z-index #:cstyle-bg-gradient
           #:cstyle-min-height #:cstyle-max-height #:cstyle-content #:cstyle-cursor
           #:cstyle-text-transform #:cstyle-visibility
           #:cstyle-letter-spacing #:cstyle-word-spacing #:cstyle-text-indent
           #:cstyle-overflow-wrap #:cstyle-word-break
           #:cstyle-bg-image #:cstyle-bg-repeat #:cstyle-bg-position #:cstyle-bg-size #:cstyle-bg-attachment
           #:cstyle-object-fit))
