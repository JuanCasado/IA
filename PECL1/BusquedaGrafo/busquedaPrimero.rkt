#lang racket
(provide (all-defined-out))
(define (insertar_siguientes_primero siguientes abiertos)
  (define (insertar_ordenado siguiente abiertos)
    (cond
      [(null? abiertos) (list siguiente)]
      [(> (car (reverse (car abiertos))) (car (reverse siguiente))) (cons siguiente abiertos)]
      [else (cons (car abiertos) (insertar_ordenado siguiente (cdr abiertos)))]
    )
  )
  (if (null? siguientes) abiertos
     (insertar_siguientes_primero (cdr siguientes) (insertar_ordenado (car siguientes) abiertos))
  )
)