#lang racket


;ejercios de expresiones básicas clase
(define (expresion1 x) (- (+ (/ (* 2 x) 5) 3) 6))
(define (expresion2 y) (+ 4 (* 5 y)))
(define (expresion3 t y x z) (+ (+ z 1) (* t (* x (log y)))))

;EJ 1
(writeln "-------------------EJ 1-------------------")
(+ 1 2 3 4)
(+ 5)
(+)

(* 1 2 3 4)
(* 2)
(*)

(/ 6 2 3 10)
(/ 5)
;(/) --> ERROR!

;EJ 2
(writeln "-------------------EJ 2-------------------")
(define (op1 x y) (+ x y (* 3 x y)))
(op1 2 5)
(exp (* +i pi 74))
(define (op3 r) (* 4/3 pi (expt r 3)))
(op3 5)
(define (op4 l G) (* 2 pi (sqrt (/ l G))))
(op4 2 5)

;EJ 3
(writeln "-------------------EJ 3-------------------")
(sqrt 3)
(expt 3 1/2)
(log 3)
(log 2 10)

;EJ 4
(writeln "-------------------EJ 4-------------------")
(define (rad2grad rad) (* (/ 180 pi) rad))
(rad2grad 0.846)

;EJ 5
(writeln "-------------------EJ 5-------------------")
(exp 1)

;EJ 6
(writeln "-------------------EJ 6-------------------")
(define (logic1 x y z) (or y (and (not y) z #t)))
(logic1 #t #f #t)
(logic1 #t #f #f)
(define (logic2 x y z) (and (or x z) (not (or #t y))))
(logic2 #t #f #t)
(logic2 #t #f #f)

;EJ 7
(writeln "-------------------EJ 7-------------------")

(define (l1 a b) (and a b))
(writeln "a b r")
(write "0 0")
(l1 #f #f)
(write "0 1")
(l1 #f #t)
(write "1 0")
(l1 #t #f)
(write "1 1")
(l1 #t #t)
(define (l2 a b) (or a b))
(writeln "a b r")
(write "0 0")
(l2 #f #f)
(write "0 1")
(l2 #f #t)
(write "1 0")
(l2 #t #f)
(write "1 1")
(l2 #t #t)

;EJ 8
(writeln "-------------------EJ 8-------------------")
(define (traductor_notas a)
  
;(cons
; [(< a 5) (writeln "suspoenso")]
; [(> a 5) (writeln "suspoenso")]))
