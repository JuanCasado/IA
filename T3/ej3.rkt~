#lang racket

;mcd por algoritmo de euclides
(define (mcd a b)
  (cond
    ((= a 0) b)
    ((= b 0) a)
    ((> b a) (mcd a (- b a)))
    (else (mcd b (- a b)))
  )
)

;redefinir primitiva xên de for forma distintas
(define (pot base e)
  (cond ((= e 0) 1)
        (else (* base (pot base (- e 1))))
   )
)

(define (pot_r base e)
  (cond ((= e 1) base)
        ((even? e) (* (pot_r base (/ e 2)) (pot_r base (/ e 2))))
        (else (* base (pot_r base (- e 1))))
   )
)

;suma de naturales
(define (sumn n)
  (if (= n 0) 0
      (+ n (sumn (- n 1)))
  )
)

;suma de cuadrados
(define (sumc c)
  (if (= c 0) 0
      (+ (* c c) (sumc (- c 1)))
  )
)

;maximo divisor
(define (max_div_aux n i)
  (if (= (modulo n i) 0)
       i
       (max_div_aux n (- i 1))
  )
)
(define (max_div n)
  (max_div_aux n (- n 1))
)

;natural en decimal
(define (natondec n d)
  (let* ([nofd (floor d)]
        [next_d (*(- d nofd)10)])
    (cond
        ((= nofd 0) 'no)
        ((= nofd n) 'si)
        (else (natondec n next_d)))
    )
)