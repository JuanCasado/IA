#lang racket

;EJ 1
(writeln "-------------------EJ 1-------------------")
(car '(((por)) (que) (no) esta))

;EJ 2
(writeln "-------------------EJ 2-------------------")
(car '(((hoy)) (no) (es) jueves))

;EJ 3
(writeln "-------------------EJ 3-------------------")
(cons '(a b) '(c d))
(list '(a b) '(c d))
(append '(a b) '(c d))
(car (car '(((hoy))(no))))
(caar '(((hoy))(no)))
(cdr '(((hoy))(no)))
(car '(((hoy))(no)))
(cdar '(((hoy))(no)))

(writeln "#noHay4")

;EJ 5
(writeln "-------------------EJ 5-------------------")
(define (poner_en_cola a l) (reverse (cons a (reverse l))))
(define (poner_en_principio a l) (cons a l))
(define (poner_en_segundo a l) (cons (car l) (cons a (cdr l))))

;EJ 6
(writeln "-------------------EJ 6-------------------")
(define (primero l)  (car l))
(define (segundo l)  (second l))
(define (tercero l)  (third l))
(define (ultimo l)  (car (reverse l)))
(define (penultimo l)  (second (reverse l)))
(define (antepenultimo l)  (third (reverse l)))
(define (all l) (cons (primero l) (cons(segundo l) (cons (tercero l) (cons (ultimo l) (cons (penultimo l) (cons (antepenultimo l) '())))))))
(all '(1 2 3 4 5 6 7 8 9 0))

;EJ 7
(writeln "-------------------EJ 7-------------------")
(define (tiene_dos l) (> (length l) 1))

;EJ 8
(writeln "-------------------EJ 8-------------------")
(define (longitud l) (if (null? l) 0 (+ 1 (longitud (cdr l)))))

;EJ 9
(writeln "-------------------EJ 9-------------------")
(define (da_la_vuelta l) (if (null? l) '() (append (da_la_vuelta (cdr l)) (cons (car l) '()))))

;EJ 10
(writeln "-------------------EJ 10-------------------")
(define (suma_lista l) (if (null? l) 0 (+ (car l) (suma_lista (cdr l)))))



