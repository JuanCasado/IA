#lang racket

#| Ejercicio 7 - No son conmutativos |#
(or (> 7 3) (3 * +))

#| Ejercicio 8 |#
(define (calificacion nota)
  (cond
    [(< nota 5) "Suspenso"]
    [(< nota 7) "Aprobado"]
    [(< nota 9) "Notable"]
    [else "Sobresaliente"]
  )
)
(calificacion -10)
(calificacion 8)
(calificacion 9.5)

#| Ejercicio 9 |#
(define (dado)
  (+ 1 (random  5))
)
(dado)

#| Ejercicio 10 - (ax^2 + bx+ c) |#
(define (raices-polinomio a b c)
  (let ([discriminante (- (sqr b) (* 4 a c))])
    (cond
      [(< discriminante 0) 0]
      [(= discriminante 0) 1]
      [(> discriminante 0) 2]
    )
  )
)
(raices-polinomio 1 -2 1)

#| Ejercicio 11 |#
(define (positivo? numero)
  (> numero 0)
)
(positivo? -1)

#| Ejercicio 12 |#
(define (num-positivo? numero)
  (and (number? numero)
       (> numero 0)
  )
)
(num-positivo? "a")

#| Ejercicio 14 - (ax^3 + bx^2 + cx + d) |#
(define (ecuacion-cubica x a b c d)
  (+ (* a (* x x x)) (* b (* x x)) (* c x) d)
)
(ecuacion-cubica 0 1 -4 -3 -10)
