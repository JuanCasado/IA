#lang racket
(define prueba '(4 6 10 9))


(define (obtener_actual abiertos) (cons (car abiertos) '()))

(define (obtener_resto_abiertos abiertos) (cdr abiertos))
(define (obtener_sucesores actual matriz) 0)


(define (main ciudad1 ciudad2) ((obtener_resto_abiertos(obtener_sucesores obtener_actual(ciudad1) matriz)))%Lammar a las 3 funciones clave... revisarlo bien