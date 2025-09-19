(import test)

(test-case "test" equal? (list 1 2 3 4) (lambda () (list 1 2 3 4)))
