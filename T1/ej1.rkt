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
(cond
 [(< a 5) (writeln "suspoenso")]
 [(< a 7) (writeln "aprobado")]
 [(< a 9) (writeln "notable")]
 [(< a 10) (writeln "sobresaliente")]
 [(< a +inf.0) (writeln "matricula")]))
(write "1 --> ")
(traductor_notas 1)
(write "6 --> ")
(traductor_notas 6)
(write "8 --> ")
(traductor_notas 8)
(write "9 --> ")
(traductor_notas 9)
(write "10 --> ")
(traductor_notas 10) 

;EJ 9
(writeln "-------------------EJ 9-------------------")
(define (lanzar_dado) (+ 1(random 6)))
(lanzar_dado)

;EJ 10
(writeln "-------------------EJ 10------------------")
(define (x_vertex a b) (/ (- b) (* 2 a)))
(define (y_cuadratic a b c x) (+ (* a (expt x 2)) (* b x) c))
(define (count_cuadratic_solutions a b c)
  (if (> a 0)
      (cond
        [(> (y_cuadratic a b c (x_vertex a b)) 0) 0]
        [(= (y_cuadratic a b c (x_vertex a b)) 0) 1]
        [(< (y_cuadratic a b c (x_vertex a b)) 0) 2])
      (cond
        [(> (y_cuadratic a b c (x_vertex a b)) 0) 2]
        [(= (y_cuadratic a b c (x_vertex a b)) 0) 1]
        [(< (y_cuadratic a b c (x_vertex a b)) 0) 0])))

(count_cuadratic_solutions 1/4 2 3)

;EJ 11
(writeln "-------------------EJ 11------------------")
(define (es_positivo a) (> a 0))

;EJ 12
(writeln "-------------------EJ 12------------------")
(define (es_numero_positivo a) (and (number? a) (> a 0)))

;EJ 13
(writeln "-------------------EJ 13------------------")
(define (es_numero_entero_positivo a) (and (number? a) (integer? a) (> a 0)))

;EJ 14
(writeln "-------------------EJ 14------------------")
(define (polinomio_cubico a b c d x)
  (+ (* a (expt x 3))
     (* b (expt x 2))
     (* c x)
     d))
(define (polinomio_cubico_horner a b c d x) (+ d (* x (+ c (* x (+ b (* x a)))))))



  