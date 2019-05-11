#lang racket
(require "object.rkt")
(require "minimax.rkt")

(define (ask question)
      (display question)
      (display "\n")
      (string->number(read-line (current-input-port)))
)
(define (ask-str question)
      (display question)
      (display "\n")
      (string-downcase(read-line (current-input-port)))
)

(define (remove node where ammount)
  (let [(formated-ammount (validate-number ammount))]
  (cond
    [(string=? where "x") (rootChild (-(getX node) formated-ammount) (getY node) (getZ node))]
    [(string=? where "y") (rootChild (getX node) (-(getY node) formated-ammount) (getZ node))]
    [(string=? where "z") (rootChild (getX node) (getY node) (-(getZ node) formated-ammount))]
    [else  (rootChild (getX node) (getY node) (getZ node))] 
  ))
)

(define (play)
  (define (ganador turno) 
    (if turno (display "HA GANADO EL JUGADOR\n") (display "HA GANADO LA MAQUINA\n"))
  )
  (define (bucle-juego nodo turno)
    (if turno (display "TURNO DE LA MAQUINA\n") (display "TURNO DEL JUGADOR\n"))
    (draw nodo)
    (if (isLeaf nodo)
        (ganador turno)
        (if turno
            (bucle-juego (minmax nodo) (not turno))
            (bucle-juego (remove nodo (ask-str "Introduce de dónde eliminar: ") (ask "Introduce cuanto quitar: ")) (not turno))
   )))
   (bucle-juego (rootChild (ask "Introduce la X: ") (ask "Introduce la Y: ") (ask "Introduce la Z: ")) (>(random) 0.5))
)