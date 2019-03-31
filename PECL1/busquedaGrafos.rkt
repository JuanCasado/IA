#lang racket
(provide (all-defined-out))

(define (obtenerSiguientes ciudad ciudades)
  (cond
    [(null? ciudades) '()]
    [(equal? ciudad (caar ciudades)) (cdar ciudades)]
    [else (obtenerSiguientes ciudad (cdr ciudades))]
  )
)

(define (eliminarCerrados abiertos cerrados)
  (define (eliminarCerrado abiertos cerrado)
    (cond
      [(null? abiertos) '()]
      [(string=? (obtenerUltimo (car abiertos)) cerrado) (cdr abiertos)]
      [else (cons (car abiertos) (eliminarCerrado (cdr abiertos) cerrado))]
    )
  )
  
  (cond
    [(null? cerrados) abiertos]
    [else (eliminarCerrados (eliminarCerrado abiertos (car cerrados)) (cdr cerrados))]
  )
)
(define (obtenerPrimeroAbiertos abiertos) (car abiertos))

(define (aumentarCamino camino ciudad)
  (reverse (cons (+ (car (reverse camino)) (cadr ciudad)) (cons (car ciudad) (cdr (reverse camino))))))

(define (eliminarPrimeroAbiertos abiertos) (cdr abiertos))

(define (obtenerUltimo l) (cadr (reverse l)))

(define (ha_finalizado objetivo actual)
  (string=? (car (reverse objetivo)) (obtenerUltimo actual))
)

