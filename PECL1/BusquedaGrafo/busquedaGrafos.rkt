#lang racket
(provide (all-defined-out))

;De un nodo actual se obtiene la lista de siguientes
(define (obtenerSiguientes ciudad ciudades)
  (cond
    [(null? ciudades) '()]
    [(equal? ciudad (caar ciudades)) (cdar ciudades)]
    [else (obtenerSiguientes ciudad (cdr ciudades))]
  )
)

;De una lista de siguientes se eliminan aquellos que nos lleven a un nodo perteneciente a una lista de cerrados
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

;Se obtiene el primero de los elementos de la lista de abiertos
(define (obtenerPrimeroAbiertos abiertos) (car abiertos))

;Añade el elemento actual a cada elemento de la lista de siguentes para indicar que desde ese actual se llegó a ese siguiente
(define (aumentarCaminos siguientes actual)
  (define (aumentarCamino camino actual)
    (reverse (cons (+ (car (reverse camino)) (car (reverse actual))) (reverse (append (reverse (cdr (reverse actual))) (reverse (cdr (reverse camino))))))))
  (cond
    [(null? siguientes) '()]
    [else (cons (aumentarCamino (car siguientes) actual) (aumentarCaminos (cdr siguientes) actual))]
  )
)

;Elimina el primer elemento de la lista de abiertos
(define (eliminarPrimeroAbiertos abiertos) (cdr abiertos))

;Obtiene el ultimo elemento de un camino
(define (obtenerUltimo l) (cadr (reverse l)))

;Comprueba que el algoritmo haya llegado al destino deseado
(define (ha_finalizado objetivo actual)
  (string=? (car (reverse objetivo)) (obtenerUltimo actual))
)

