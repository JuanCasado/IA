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
    [(string=? where "x") (if (<= (getX node) 1) (remove node "y" ammount)(rootChild (-(getX node) formated-ammount) (getY node) (getZ node)))]
    [(string=? where "y") (if (<= (getY node) 1) (remove node "z" ammount)(rootChild (getX node) (-(getY node) formated-ammount) (getZ node)))]
    [(string=? where "z") (if (<= (getZ node) 1) (remove node "x" ammount)(rootChild (getX node) (getY node) (-(getZ node) formated-ammount)))]
    [else  (rootChild (getX node) (getY node) (getZ node))] 
  ))
)

(define (play)
  (define (ganador turno) 
    (if turno (display "\nHA GANADO EL JUGADOR\n") (display "\nHA GANADO LA MAQUINA\n"))
  )
  (define (bucle-juego min-max lazzy cache infer nodo turno)
    (if turno (display "\nTURNO DE LA MAQUINA\n") (display "\nTURNO DEL JUGADOR\n"))
    (draw nodo)
    (if (isLeaf nodo)
        (ganador turno)
        (if turno
            (if min-max
                (bucle-juego min-max lazzy cache infer (minmax nodo lazzy infer cache) (not turno))
                (bucle-juego min-max lazzy cache infer (alphabeta nodo lazzy infer cache) (not turno))
            )
            (let [(user (ask-str "Introduce de dónde eliminar: "))]
              (if (string=? user "auto")
                  (if min-max
                      (bucle-juego min-max lazzy cache infer (minmax nodo lazzy infer cache) (not turno))
                      (bucle-juego min-max lazzy cache infer (alphabeta nodo lazzy infer cache) (not turno))
                  )
                  (bucle-juego min-max lazzy cache infer (remove nodo user (ask "Introduce cuanto quitar: ")) (not turno))
            ))
   )))
   (bucle-juego (string=? (ask-str "MODO DE JUEGO (ALPHABETA|MINMAX)") "minimax")
                (string=? (ask-str "USO DE MEMORIA (LAZZY|COMPLETE)") "lazzy")
                (not(string=? (ask-str "USO DE MEMORIA (CACHE|SIMPLE)") "simple"))
                (string=? (ask-str "INFERENCIA DE NODOS TERMINALES (INFERE|LEAF)") "infere")
                (rootChild (ask "Introduce la X: ") (ask "Introduce la Y: ") (ask "Introduce la Z: "))
                (>(random) 0.5))
)