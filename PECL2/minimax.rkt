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
    (newChild x y z alpha beta (not (getType nodo)))
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
  (> 1 (getChildCount nodo))
)

;Actualiza un nodo hijo, pone en su valor quien gana
(define (update-leaf nodo)
    (define (value nodo) (if (getType nodo) 1 -1))
    (setW nodo (value nodo))
)

;Actualiza un nodo, su alpha o beta según corresponda, su peso y cambia su id por la de su mejor hijo
(define (update root-node child)
  (define (update-max root-node child);TOMAR MAYOR PESO
    (let* [
           (action (> (getAlpha root-node) (getW child)))
           (val (if action (getAlpha root-node) (getW child)))
           (id (if action (getId root-node) (getId child)))
           (other (getBeta child))
          ]
      (setID (setBeta (setAlpha (setW root-node val) val) other) id)
  ))
  (define (update-min root-node child);TOMAR MENOR PESO
    (let* [
           (action (< (getBeta root-node) (getW child)))
           (val (if action (getBeta root-node) (getW child)))
           (id (if action (getId root-node) (getId child)))
           (other (getAlpha child))
          ]
      (setID (setAlpha (setBeta (setW root-node val) val) other) id)
  ))
  (display "\nU")
  (display (if (getType root-node) (update-max root-node child) (update-min root-node child)))
  (if (getType root-node) (update-max root-node child) (update-min root-node child))
)

;MINIMAX
(define (minmax nodo)
  (define (profundidad node)
    (display "\np")
    (display node)
    (if (isLeaf node) (update-leaf node) (anchura node (createChildren node)))
  )
  (define  (anchura root-node node-list)
    (display "\nA")
    (display root-node)
    (display "\nL")
    (display node-list)
    (if (null? node-list) root-node
        (if (>= (getAlpha root-node) (getBeta root-node)) root-node
        ;(if #f root-node
          (anchura (update root-node (profundidad (car node-list))) (cdr node-list)))
   ))
  (profundidad nodo)
)





