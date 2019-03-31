#lang racket
(provide (all-defined-out))
(define (insertar_siguientes_anchura siguientes abiertos)
  (if (null? siguientes) abiertos
      (insertar_siguientes_anchura (cdr siguientes) (reverse (cons (car siguientes) (reverse abiertos))))))


