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


;EJ 11
(writeln "-------------------EJ 11-------------------")
(define (cuadrados_lista l) (if (null? l) 0 (+ (expt (car l) 2) (cuadrados_lista (cdr l)))))

;EJ 12
(writeln "-------------------EJ 12-------------------")
(define (is_aritmetic l) (if (> 3 (length l)) #t (and (= (- (first l) (second l))(- (second l) (third l))) (is_aritmetic (cdr l)))))
(define (is_geometric l) (if (> 3 (length l)) #t (and (= (/ (first l) (second l))(/ (second l) (third l))) (is_geometric (cdr l)))))
(define (is_progresion l) (if (is_aritmetic l) (if (is_geometric l) "aritmetica y geometrica" 'aritmetica) (if (is_geometric l) 'geometrica "ni geometrica ni aritmetica")))

;EJ 13
(writeln "-------------------EJ 13-------------------")
(define (random_in l) (if (= 0 (length l)) 0 (random (length l))))
(define (delete_element l n) (if (= n 0) (cdr l) (cons (car l) (delete_element (cdr l) (- n 1)))))
(define (_distribuir monitores jugadores index_jugador index_monitor save) (if (null? jugadores) '() (append save (cons (list (list-ref monitores index_monitor) (list-ref jugadores index_jugador))
                                                                              (_distribuir monitores (delete_element jugadores index_jugador) (random_in (cdr jugadores))(remainder (+ 1 index_monitor)(length monitores))save)))))
(define (distribuir monitores jugadores) (_distribuir monitores jugadores (random_in jugadores) 0 '()))

;EJ 14
(writeln "-------------------EJ 14-------------------")


;EJ 15
(writeln "-------------------EJ 15-------------------")
(define (reverse_string str)(list->string (reverse (string->list str))))

;EJ 16
(writeln "-------------------EJ 16-------------------")
(define (palindromo str) (if (> 2 (string-length str)) #t (and (eq? (car (string->list str)) (car (reverse (string->list str)))) (palindromo (list->string(cdr (reverse(cdr (string->list str)))))))))


