import { scamperTest } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/127

scamperTest('normal-let', `
  (define f
    (lambda ()
      (let ([x (+ 1 1)]
            [y (+ 10 1)]
            [z (+ 100 1)])
           (+ x (* y z)))))
  (f)
`, [
  '1113'
])

scamperTest('let-shadowing', `
  (define sample3
    (lambda (x)
      (list x
            (let ([x (+ x 1)]
                  [y (+ x 1)])
            (list x y)))))

  (sample3 10)
`, [
  '(list 10 (list 11 11))'
])

scamperTest('let-telescoping', `
  (define sample3
    (lambda (x)
      (list x
            (let* ([x (+ x 1)]
                   [y (+ x 1)])
            (list x y)))))

  (sample3 10)
`, [
  '(list 10 (list 11 12))'
])