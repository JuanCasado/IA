#lang racket

(if (= 2 2) "hola" "adios")

(if (positive? (sqrt 4))
    "sí es positivo"
    "no es positivo"
)

(and (< 3.1416 (expt 10.1424 3.8)) (negative? pi))

(define (responder-saludo s)
  (if (string? s)
      (if (equal? "hola" (substring s 0 4))
          "¡hola, gusto de verte!"
          "¿perdón?"
      )
      "perdón, ¿qué?"
  )
)

(define (responder-saludo2 s)
  (if (and (string? s)
           (>= (string-length s) (string-length "hola"))
           (equal? "hola" (substring s 0 4))
      )
      "¡hola, gusto de verte!"
      "perdón, ¿qué?"
  )
)

(define (responder-mas s)
  (cond
    [(equal? "hola" (substring s 0 4)) "¡hola, gusto de verte!"]
    [(equal? "adiós" (substring s 0 5)) "¡nos vemos, que te vaya bien!"]
    [(and (equal? "¿" (substring s 0 1))
          (equal? "?" (substring s (- (string-length s) 1)))
     ) "No sé"]
    [else "perdón, ¿qué?"]
  )
)

(define (duplicar valor)
  ((if (string? valor) string-append +) valor valor)
)
