#lang racket

; (dot_product 3 4 -8 6) --> 0
; (dot_product 1 1 1 0) --> 1
(define (dot_product ax ay bx by) (+ (* ax bx) (* ay by)))

; (module 1 1) --> 1.4142135623730951
; (module 0 5) --> 5
(define (module x y) (sqrt (+ (expt x 2) (expt y 2))))

; (rad2grad 1.58) --> 90.52733163067008
; (rad2grad 1.57) --> 89.95437383553924
(define (rad2grad rad) (* (/ 180 pi) rad))

; (vector_angle 1 0 1 0) --> 0
; (vector_angle 1 0 0 1) --> 90
; (vector_angle 1 1 1 0) --> 45.00000000000001
(define (vector_angle ax ay bx by) (rad2grad (acos (/ (dot_product ax ay bx by) (* (module ax ay) (module bx by))))))




