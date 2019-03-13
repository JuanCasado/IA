#lang racket

;-----------------------------------------------------------------------
;Area del triángulo según sus lados
(define (area_t a b c)
  (let (
        (m (/ (+ a b c ) 2))
       )
       ( sqrt(* m (- m a) (- m b) (- m c)))
    )
  )

;-----------------------------------------------------------------------
;El alcance de una variable queda acotado por el cuerpo del let
(define (p x y)
  (let ((x_c (* x x))
        (y_c (* y y))
       )
        (- (* x_c y_c) x_c y_c)
    )
  )

;-----------------------------------------------------------------------
;El alcance de un let definido está SOLO en el cuero,
;No se pueden crear dependencias de let en su declaración
;Para ello está let* que si lo permite

;NO FUNCIONA
;(define (q x)
;  (let (
;        (x_c (* x x))
;        (x_cc (* x_c x_c))
;        )
;        (+ x_cc (* 2 x_c))
;    )
;  )

;SI FUNCIONA
(define (q x)
  (let* (
        (x_c (* x x))
        (x_cc (* x_c x_c))
        )
        (+ x_cc (* 2 x_c))
    )
  )

;-----------------------------------------------------------------------
;Composicion de funciones
(define (composicion f g)
  (lambda (x) (g (f x))))

;-----------------------------------------------------------------------
(define (show_let)
  (let ((x 3)) 
          (let ((x 5)
                (y x));La y vale 3 pues toma el valor de la x de fuera
            (+ x y);La x vale 5 pues toma el valor de la x de dentro
  ))
)
;-----------------------------------------------------------------------
(define (f1 e)
  ((lambda (x) (+ x x)) e) 
)

;((lambda (y) (+ y 3)) ((lambda (z) (* 3/4 ((lambda (t) (- 2 t)) v) )) y))

;No importa el nombre de las variables porque se ven ocultas por los efectos de los lambda
;Solo importa el valor del literal libre.
(define (f2 x)
((lambda (x) (+ x 3)) ((lambda (x) (* 3/4 ((lambda (x) (- 2 x)) x) )) x))
)

;((x -2) * 3/4) +3








