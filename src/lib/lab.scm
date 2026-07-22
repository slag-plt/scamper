;;; (html? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an HTML element.
(define html? (js-var "html_isElement"))

;;; (title text) -> html?
;;;  text : string?
;;; Returns a title element.
(define title (js-var "lab_title"))

;;; (part text) -> html?
;;;  text : string?
;;; Returns a part element.
(define part (js-var "lab_part"))

;;; (problem text) -> html?
;;;  text : string?
;;; Returns a problem element.
(define problem (js-var "lab_problem"))

;;; (description text) -> html?
;;;  text : string?
;;; Returns a description element.
(define description (js-var "lab_description"))
