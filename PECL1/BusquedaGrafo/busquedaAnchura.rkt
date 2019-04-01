#lang racket
(provide (all-defined-out))

;Los siguentes se insertan al final de la lista de abiertos
(define (insertar_siguientes_anchura siguientes abiertos)
  (if (null? siguientes) abiertos
      (insertar_siguientes_anchura (cdr siguientes) (reverse (cons (car siguientes) (reverse abiertos))))))


