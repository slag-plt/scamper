;;; (html? v) -> boolean?
;;;  v : any
;;; Returns `#t` if and only if `v` is an HTML element.
(define html? (js-var "html_isElement"))

;;; (title text) -> html?
;;;  text : string?
;;; Returns a title element.
;;; @category formatting, html?, part, problem, description
(define title (js-var "lab_title"))

;;; (part text) -> html?
;;;  text : string?
;;; Returns a part element.
;;; @category formatting, html?, title, problem, description
(define part (js-var "lab_part"))

;;; (problem text) -> html?
;;;  text : string?
;;; Returns a problem element.
;;; @category formatting, html?, title, part, description
(define problem (js-var "lab_problem"))

;;; (description text) -> html?
;;;  text : string?
;;; Returns a description element.
;;; @category formatting, html?, title, part, problem
(define description (js-var "lab_description"))
