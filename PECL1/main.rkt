#lang racket
#|
(require racket/include)
(include "grafo.rkt")
(include "busquedaGrafos.rkt")
(include "busquedaAnchura.rkt")
|#
(define (inicioBusqueda objetivo ciudades tipo_busqueda)
  (cond
    [(string=? tipo_busqueda "anchura")(write "busqueda anchura")]
    [(string=? tipo_busqueda "profundidad")(write "busqueda anchura")]
    [(string=? tipo_busqueda "primero_mejor")(write "busqueda anchura")]
  )
)

(define (ha_finalizado objetivo actual)
  (string=? (car (reverse objetivo)) (cadr (reverse actual)))
)