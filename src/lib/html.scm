;;; (element? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an HTML element.
(define element? (js-var "html_isElement"))

;;; (text-area? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a text area.
(define text-area? (js-var "html_textAreaQ"))

;;; (text-area id) -> text-area?
;;;  id : string?
;;; Creates a text area with the given id.
;;; @category html
(define text-area (js-var "html_textArea"))

;;; (text-area-get text-area) -> string?
;;;  text-area : text-area?
;;; Returns the text in the given text area.
;;; @category html
(define text-area-get (js-var "html_textAreaGet"))

;;; (button? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is a button.
(define button? (js-var "html_buttonQ"))

;;; (button label fn) -> button?
;;;  label : string?
;;;  fn : procedure?
;;; Creates a button with the given label and function that is called when the button is pressed.
;;; @category html
(define button (js-var "html_button"))

;;; (tag name . c) -> element?
;;;  name : string?
;;;  c : any
;;; Creates an HTML element with the given name and children.
;;; @category html
(define tag (js-var "html_tag"))

;;; (tag-set-children! elt . c) -> element?
;;;  elt : any
;;;   an HTML element
;;;  c : any
;;;   an HTML element or string
;;; Sets `elt`'s children to be `c1`, `c2`, ..
;;; @category html, mutation, predicates
(define tag-set-children! (js-var "html_tagSetChildren"))

;;; (on-keydown! fn) -> void?
;;;  fn : procedure?
;;; Calls `fn` whenever a key is pressed while the page is focused. `fn` takes a single argument, the key pressed by the user as a string.
;;; @category html, mutation, predicates
(define on-keydown! (js-var "html_onKeydown"))
