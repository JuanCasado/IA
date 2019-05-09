#lang racket
(provide (all-defined-out))
(require "cube.rkt")


;POSICIONES DE LOS ELEMENTOS
(define iId       1)
(define iPath     2)
(define iAlpha    3)
(define iBeta     4)
(define iWeight   5)
(define iType     6)
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

(define (getX nodo)(car(getId nodo)))
(define (getY nodo)(cadr(getId nodo)))
(define (getZ nodo)(caddr(getId nodo)))

(define (getNX nodo)(- (getX nodo) 1))
(define (getNY nodo)(- (getY nodo) 1))
(define (getNZ nodo)(- (getZ nodo) 1))

;Devuelve una lista de caminos
(define (getPath nodo) (get nodo iPath))

;Pone una lista de caminos en un nodo
(define (putPath nodo path)
  (establish nodo path iPath)
)

;Aumenta la lista de caminos
(define (extendPath nodo id)
  (putPath nodo (reverse (cons id (reverse (get nodo iPath)))))
)

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

;Devuelve el tipo (MAX o MIN) de un nodo
(define (getType nodo) (get nodo iType))

;Devuelve cuantos hijos tiene un nodo
(define (getChildAmount nodo)
  (length (get nodo iPath))
)

;Generador de hijos
(define (newChild x y z alpha beta tipo)
  (list (list x y z) '() alpha beta 0 tipo)
)
(define (rootChild x y z tipo)
  (list (list x y z) '() -inf.0 +inf.0 0 tipo)
)

;Pinta un nodo
(define (draw nodo)
  (lcube (getX nodo) (getY nodo) (getZ nodo))
)