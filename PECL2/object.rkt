#lang racket

(define a '((3 4 5) ((3 4 5) (3 4 5) (3 4 5)) 333 55 8 #f (("id2" ((3 4 5) (3 4 5)) 666 99 7 #t ()) ("id3" ((3 4 5)) 373 575 98 #f ()))))
(define b '((8 6 3) ((3 4 5) (3 4 5) (3 4 5)) 333 55 8 #f ()))
(define c '((1 1 1) ((3 4 5) (3 4 5)) 666 99 7 #t ()))

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

;Añade un nuevo hijo al nodo
(define (addChild nodo nodoHijo)
  (set nodo (list (cons nodoHijo (reverse (get nodo iChildren)))) iChildren)
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
(define (newChild id alpha beta tipo)
  (list id '() alpha beta 0 tipo '())
)
(define (rootChild x y z tipo)
  (list (list x y z) '() -inf.0 +inf.0 0 tipo '())
)

;Genera el siguiente hijo dependiendo de la cantidad de hijos ya generados
(define (nextChild nodo index)
  (print 0)
)