#lang racket

(define a '("id1" (1 2 3) 333 55 8 #f (("id2" (4 9) 666 99 7 #t '()) ("id3" (8) 373 575 98 #f '()))))
(define posPath 2)

;Devuelve la id de un nodo
(define (getId nodo) (car nodo))

;Devuelve una lista de caminos
(define (getPath nodo) (cadr nodo))

;Establece una lista de caminos
(define (setPath nodo path) (put nodo path posPath))

;Aumenta la lista de caminos
;(define (extendPath nodo))

;Devuelve el alpha de un nodo
(define (getAlpha nodo) (caddr nodo))

;Establece el alpha de un nodo
;(define (setAlpha nodo alpha))

;Devuelve el beta de un nodo
(define (getBeta nodo) (cadddr nodo))

;Establece el beta de un nodo
;(define (setBeta nodo))

;Devuelve el peso de un nodo
(define (getW nodo) (caddr (reverse nodo)))

;Establece el peso de un nodo
;(define (setW nodo w))

;Devuelve el tipo (MAX o MIN) de un nodo
(define (getType nodo) (cadr (reverse nodo)))

;Añade un nuevo hijo al nodo
;(define (addChild nodo nodoHijo))

;Genera un nuevo hijo dependiendo del id dado
;(define (nextChild nodo index))

;Devuelve cuantos hijos tiene un nodo
;(define (getChildAmount nodo))

;Obtiene un hijo ya creado dependiendo de la id dada
;(define (getChild nodo index))

;Funcion que pone un elemento en una lista en una posicion dada
(define (put list elem index) (cond
                                [(empty? list) "Las has liado chaval"]
                                [(= 1 index) (cons elem (cdr list))]
                                [else (cons (car list) (put (cdr list) elem (- index 1)))]
                               )
)

(define (pruebaTonta valor) (if valor "Si" "No"))