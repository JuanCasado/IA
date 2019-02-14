#lang racket

;FUNCIONES
#|==========================================================
   Función: cuadrado : numero -> numero
   Obj: Calcular el cuadrado de un número <n>
  ==========================================================|#
(define (cuadrado n)
  (* n n)
)

#|==========================================================
   Función: areaCirculo : numero -> numero
   Obj: Calcular el área de un círculo dado su diámetro <d>
  ==========================================================|#
(define (areaCirculo D)
  (/ (* 3.14159 (cuadrado D)) 4)
)

;PROGRAMA PRINCIPAL
(areaCirculo 10)
