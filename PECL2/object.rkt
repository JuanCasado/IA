#lang racket
(provide (all-defined-out))
(require "cube.rkt")


;POSICIONES DE LOS ELEMENTOS
(define iId       1)
(define iAlpha    2)
(define iBeta     3)
(define iWeight   4)
(define iType     5)
(define iBest     6)
(define lenNodo   6)
;Obtener un elemento de una lista en una posicion dada
(define (get list index)
  (cond
    [(empty? list) "Las has liado chaval en get"]
    [(= 1 index) (car list)]
    [else  (get (cdr list) (- index 1))]
  )
)
;Poner un elemento en una lista en una posicion dada
(define (set list elem index)
  (cond
    [(empty? list) elem]
    [(= 1 index) (cons elem (cdr list))]
    [else (cons (car list) (set (cdr list) elem (- index 1)))]
   )
)
;Quitar un elemento en una lista en una posicion dada
(define (remove list index)
  (cond
    [(empty? list) null]
    [(= 1 index) (cdr list)]
    [else (cons (car list) (remove (cdr list) (- index 1)))]
   )
)
(define (put list elem index)
  (cond
    [(empty? list) elem]
    [(= 1 index) (cons elem list)]
    [else (cons (car list) (put (cdr list) elem (- index 1)))]
   )
)
(define (establish list elem index)
  (if (< (length list) lenNodo)
      (put list elem index)
      (set list elem index)
  )
)

;Devuelve la id de un nodo
(define (getId nodo) (get nodo iId))
(define (setID nodo ID)
  (establish nodo ID iId)
)

(define (getX nodo)(car(getId nodo)))
(define (getY nodo)(cadr(getId nodo)))
(define (getZ nodo)(caddr(getId nodo)))

(define (getNX nodo)(- (getX nodo) 1))
(define (getNY nodo)(- (getY nodo) 1))
(define (getNZ nodo)(- (getZ nodo) 1))

;Devuelve el alpha de un nodo
(define (getAlpha nodo) (get nodo iAlpha))

;Establece el alpha de un nodo
(define (setAlpha nodo alpha)
  (establish nodo alpha iAlpha)
)

;Devuelve el beta de un nodo
(define (getBeta nodo) (get nodo iBeta))

;Establece el beta de un nodo
(define (setBeta nodo beta)
  (establish nodo beta iBeta)
)

;Devuelve el peso de un nodo
(define (getW nodo) (get nodo iWeight))

;Establece el peso de un nodo
(define (setW nodo w)
   (establish nodo w iWeight)
)

;Devuelve el mejor hijo de un nodo
(define (getBest nodo) (get nodo iBest))

;Establece el mejor hijo de un nodo
(define (setBest nodo best)
   (establish nodo best iBest)
)

;Devuelve el tipo (MAX o MIN) de un nodo
(define (getType nodo) (get nodo iType))

;Hace que el id de un nodo sea un valor mayor o igual a 1
(define (validate-number number) (if (< number 1) 1 number))

;Generador de hijos
(define (newChild x y z alpha beta tipo)
  (list (list (validate-number x)  (validate-number y)  (validate-number z)) alpha beta 0 tipo '())
)
(define (rootChild x y z)
  (list (list (validate-number x) (validate-number y) (validate-number z)) -inf.0 +inf.0 0 #t '())
)
(define (bestChild id)
  (list id -inf.0 +inf.0 0 #t '())
)

;Pinta un nodo
(define (draw nodo)
  (lcube (getX nodo) (getY nodo) (getZ nodo))
)