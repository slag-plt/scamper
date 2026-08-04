(define len (lambda (l) (if (null? l) 0 (+ 1 (len (cdr l))))))
(len (list 7 8))
