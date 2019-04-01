#lang racket
(provide (all-defined-out))

;Los siguentes se insertan al principio de la lista de abiertos
(define (insertar_siguientes_profundidad siguientes abiertos)
  (if (null? siguientes) abiertos
      (let ((reverse_siguientes (reverse siguientes)))
      (insertar_siguientes_profundidad (reverse(cdr reverse_siguientes)) (cons (car reverse_siguientes) abiertos)))))