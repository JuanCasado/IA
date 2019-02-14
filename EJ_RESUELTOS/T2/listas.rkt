#lang racket

;FUNCIONES
#|==========================================================
   Func: sumar : list -> int
   Obj: Suma los elementos de una lista
  ==========================================================|#
(define (sumar lista)
  (if (empty? lista) 0
      (+ (car lista) (sumar (cdr lista)))
  )
)

#|==========================================================
   Func: menor : list -> int
   Obj: Devuelve el elemento menor de una lista
  ==========================================================|#
(define (menor lista)
  (cond
    [(empty? lista) empty]
    [(empty? (cdr lista)) (car lista)]
    [(< (car lista) (car (cdr lista))) (menor (cons (car lista) (cddr lista)))]
    [else (menor (cdr lista))]
  )
)

#|==========================================================
   Func: eliminar : int list -> list
   Obj: Devuelve una lista sin el elemento <n>
  ==========================================================|#
(define (eliminar n lista)
  (cond
    [(empty? lista) empty]
    [(= n (car lista)) (cdr lista)]
    [else (cons (car lista) (eliminar n (cdr lista)))]
  )
)

#|==========================================================
   Func: ordenar : list -> list
   Obj: Devuelve una lista ordenada buscando el menor
  ==========================================================|#
(define (ordenar lista)
  (cond
    [(empty? lista) empty]
    [else (cons (menor lista) (ordenar (eliminar (menor lista) lista)))]
  )
)

#|==========================================================
   Func: invertir : list -> list
   Obj: Devuelve una lista invertida
  ==========================================================|#
(define (invertir lista)
  (cond
    [(empty? lista) empty]
    [else (append (invertir (cdr lista)) (list (car lista)))]
  )
)

;PROGRAMA PRINCIPAL
(sumar (list 0 1 2 3))
(menor (list 3 4 1 15 6 3 21 0))
(eliminar 0 (list 3 4 1 15 6 3 21 0))
(ordenar (list 3 4 1 15 6 3 21 0))
(invertir (ordenar (list 3 4 1 15 6 3 21 0)))
