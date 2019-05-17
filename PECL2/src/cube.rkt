#lang racket
(provide (all-defined-out))

(define python-path
  (let ((os (system-type 'os)))
    (cond
      [(equal? os 'windows) "U:\\Python\\Python IDE\\python.exe"]
      [else "/usr/bin/python"]
    )
  )
)

(define (cube x y z name)
  (define (print-cube x y z)
    (display "x = ")(display x)
    (display "\ny = ")(display y)
    (display "\nz = ")(display z)(display "\n")
   )
         (if (system* python-path name
                  (number->string x) (number->string y) (number->string z)) (print-cube x y z) (print "no python found"))
)

(define (ncube x y z) (cube x y z "ncube.py"))
(define (lcube x y z) (cube x y z "lcube.py"))