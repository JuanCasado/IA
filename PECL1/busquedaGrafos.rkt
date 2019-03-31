#lang racket
(require "grafo.rkt")
;(include "busquedaGrafos.rkt")
(require "busquedaAnchura.rkt")

(define (obtenerSiguientes ciudades ciudad)
  (cond
    [(null? ciudades) '()]
    [(equal? ciudad (caar ciudades)) (cdar ciudades)]
    [else (obtenerSiguientes (cdr ciudades) ciudad)]
  )
)

(define (obtenerPrimeroAbiertos abiertos) (car abiertos))

(define (eliminarPrimeroAbiertos abiertos) (cdr abiertos))




#| PRUEBAS |#
(insertar_siguientes_anchura (obtenerSiguientes ciudades "Madrid") '())
(insertar_siguientes_anchura (obtenerSiguientes ciudades "Sevilla") '(("Valladolid" 193) ("Bilbao" 395) ("Zaragoza" 325) ("Barcelona" 296)))
(obtenerPrimeroAbiertos '(("Valladolid" 193) ("Bilbao" 395) ("Zaragoza" 325) ("Barcelona" 296) ("Jaén" 242) ("Granada" 256) ("Sevilla" 125)))
(eliminarPrimeroAbiertos '(("Valladolid" 193) ("Bilbao" 395) ("Zaragoza" 325) ("Barcelona" 296) ("Jaén" 242) ("Granada" 256) ("Sevilla" 125)))
