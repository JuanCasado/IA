#lang racket

(writeln 'A)
(quote xyz)
(writeln 'B)
(define xyz 9)
(writeln xyz)
(writeln 'C)
(list (+ 2 3) (+ 3 6))
(writeln 'D)
(boolean? #t)
(writeln 'E)
(not #f)
(writeln 'F)
(list '(+ 6 7) (+ 1 2))
(writeln 'G)
(cond ((< 3 5)(- 3)) ((> 3 5)(+ 3)))
(writeln 'H)
(define (cuadratic_solutions a b c)
  (let ((delta (- (expt b 2)(* 4 a c))))
    (cond ((> delta 0) 2)
          ((= delta 0) 1)
          ((< delta 0) 0)
    )
  )
)
(cuadratic_solutions 1 -2 1)