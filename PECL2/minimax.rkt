#lang racket
(provide (all-defined-out))
(require "object.rkt")





;Devuelve cuantos hijos se pueden generar desde un nodo
(define (getChildCount nodo)
  (+ (getNX nodo) (getNY nodo) (getNZ nodo))
)

;Genera el siguiente hijo dependiendo de la cantidad de hijos ya generados segun MINIMAX
(define (nextChild nodo index)
  (let* [
          (limity (+ (getX nodo) (getY nodo)))
          (x (if (< index (getX nodo)) index (getX nodo)))
          (y (if (and (>= index (getX nodo)) (< (+ index 1) limity)) (- (+ index 1) (getX nodo)) (getY nodo)))
          (z (if (>= (+ index 1) limity) (- (+ index 2) limity) (getZ nodo)))
          (alpha (getAlpha nodo))
          (beta (getBeta nodo))
         ]
    (putPath (newChild x y z alpha beta (not (getType nodo))) (cons (getId nodo) (get nodo iPath)))
  )
)
;Genera todos los hijos de un nodo
(define (createChildren nodo)
  (define (_createChilds nodo index)
    (if (< index 1) '()
        (cons (nextChild nodo index) (_createChilds nodo (- index 1)))
    )
  )
  (_createChilds nodo (getChildCount nodo))
)

;Devuelve si un hijo es o no un nodo hoja
(define (isLeaf nodo)
  (= 0 (getChildCount nodo))
)

(define (update nodo)
  (define (update-max nodo)
    (let [(val (min (getW nodo) (getAlpha nodo)))]
      (setAlpha (setW nodo val) val))
  )
  (define (update-min nodo)
    (let [(val (min (getW nodo) (getBeta nodo)))]
      (setBeta (setW nodo val) val))
  )
  (define (update-leaf nodo)
    (define (value nodo) (if (getType nodo) 1 -1))
    (setW nodo (value nodo))
  )
  (if (isLeaf nodo)
      (update-leaf nodo)
      (if (getType nodo) (update-max nodo) (update-min nodo))
  )
)





