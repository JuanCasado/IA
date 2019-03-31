#lang racket

(require math/number-theory)

;------------------EJ1--------------------
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

;------------------EJ2--------------------
;mcd por algoritmo de euclides
(define (mcd a b)
  (cond
    ((= a 0) b)
    ((= b 0) a)
    ((> b a) (mcd a (- b a)))
    (else (mcd b (- a b)))
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

;------------------EJ3--------------------
;cifra en numero
(define (c_on_d c n)
  (let* ([first_n (remainder n 10)]
         [next_n (quotient n 10)])
    (cond
        ((= first_n c) 'si)
        ((= next_n 0) 'no)
        (else (c_on_d c next_n)))
    )
)
(c_on_d 3 34567890001221020256789);'si
(c_on_d 3 4567890001221020256789);'no

;------------------EJ4--------------------
;Colapso aditivo
(define (suma_cifras a r)
  (if (= a 0)
      r
      (suma_cifras (quotient a 10) (+ r (remainder a 10))))
  )
(define (colapso_aditivo a)
  (if (= (quotient a 10) 0)
       a
       (colapso_aditivo (suma_cifras a 0))))

;------------------EJ9--------------------
;redefinir primitiva xên de dos formas distintas
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

;------------------EJ14--------------------
;Calcular e x
(define (e x)(e_aux x 1 1 0.00000000000000000000000001))
(define (e_aux x iter acc error)
  (let ((next (/ (expt x iter) (factorial iter))))
    (if (> next error)
        (e_aux x (+ iter 1) (+ acc next) error)
        acc)))

