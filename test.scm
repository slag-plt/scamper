(define x 5)

(+ 1 (+ x (+ 3 (+ x 5))))

(define mult-3
  (lambda (x)
    (+ x (+ x x))))

(+ (mult-3 x) (mult-3 x))