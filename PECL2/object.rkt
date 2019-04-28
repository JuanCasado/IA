#lang racket
(require "cube.rkt")

(define a '((3 4 5) ((3 4 5) (3 4 5) (3 4 5)) 333 55 8 #f (("id2" ((3 4 5) (3 4 5)) 666 99 7 #t ()) ("id3" ((3 4 5)) 373 575 98 #f ()))))
(define b '((8 6 3) ((3 4 5) (3 4 5) (3 4 5)) 333 55 8 #f ()))
(define c '((1 1 1) () 6 99 7 #t ()))

;POSICIONES DE LOS ELEMENTOS
(define iId       1)
(define iPath     2)
(define iAlpha    3)
(define iBeta     4)
(define iWeight   5)
(define iType     6)
(define iChildren 7)
(define lenNodo   7)
;Obtener un elemento de una lista en una posicion dada
(define (get list index)
  (cond
    [(empty? list) "Las has liado chaval"]
    [(= 1 index) (car list)]
    [else  (get (cdr list) (- index 1))]
  )
)
;Poner un elemento en una lista en una posicion dada
(define (put list elem index)
  (cond
    [(empty? list) elem]
    [(= 1 index) (cons elem (cdr list))]
    [else (cons (car list) (put (cdr list) elem (- index 1)))]
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
(define (set list elem index)
  (put (remove list index) elem index)
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

(define (addChild nodo nodoHijo)
  (set nodo (list (reverse (cons nodoHijo (reverse (get nodo iChildren))))) iChildren)
)

;Devuelve cuantos hijos tiene un nodo
(define (getChildAmount nodo)
  (length (get nodo iPath))
)

;Obtiene un hijo ya creado dependiendo del indice dado
(define (getChildIndex nodo index)
  (get (get nodo iChildren) index)
)

;Obtiene un hijo ya creado dependiendo de la id dada
(define (getChildId nodo id)
  (define (_getChildId ChildList id)
    (cond
      [(empty? ChildList) "Las has liado chaval"]
      [(string=? (getId(car ChildList)) id) (car ChildList)]
      [else  (_getChildId (cdr ChildList) id)]
    )
  )
  (_getChildId (get nodo iChildren) id)
)

;Generador de hijos
(define (newChild x y z alpha beta tipo)
  (list (list x y z) '() alpha beta 0 tipo '())
)
(define (rootChild x y z tipo)
  (list (list x y z) '() -inf.0 +inf.0 0 tipo '())
)

;Pinta un nodo
(define (draw nodo)
  (lcube (getX nodo) (getY nodo) (getZ nodo))
)

;Devuelve cuantos hijos se pueden generar desde un nodo
(define (getChildCount nodo)
  (+ (getNX nodo) (getNY nodo) (getNZ nodo))
)
;Genera el siguiente hijo dependiendo de la cantidad de hijos ya generados
(define (nextChild nodo index)
  (let* [
          (limity (+ (getX nodo) (getY nodo)))
          (x (if (<= index (getX nodo)) index (getX nodo)))
          (y (if (and (> index (getX nodo)) (<= index limity)) (- index (getX nodo)) (getY nodo)))
          (z (if (> index limity) (- index limity) (getZ nodo)))
          (alpha (getAlpha nodo))
          (beta (getBeta nodo))
         ]
    (putPath (newChild x y z alpha beta (not (getType nodo))) (cons (getId nodo) (get nodo iPath)))
  )
)
;Genera todos los hijos de un nodo
(define (createChildren nodo)
  (define (_creteChilds nodo index)
    (if (< index 1)
        nodo
        (_creteChilds (addChild nodo (nextChild nodo index)) (- index 1))
    )
  )
  (_creteChilds nodo (getChildCount nodo))
)