#lang racket

;FUNCIONES
#|==========================================================
   Func: media : list -> int
   Obj: Devuelve la media de los valores de la lista
  ==========================================================|#
(define (media lista)
  (/ (apply + lista) (length lista))
)

;PROGRAMA PRINCIPAL
(media 0 1 2 3 4 5)
