#lang racket
(provide (all-defined-out))
(require "object.rkt")
(require "metadata.rkt")


;Devuelve cuantos hijos se pueden generar desde un nodo
(define (getChildCount nodo)
  (+ (getNX nodo) (getNY nodo) (getNZ nodo))
)

;Genera el siguiente hijo dependiendo de la cantidad de hijos ya generados segun MINIMAX
(define (nextChild nodo index)
  (let* [
          (limity (+ (getX nodo) (getY nodo)))
          (x (if (< index (getX nodo)) index (getX nodo)))
          (y (if (and (>= index (getX nodo)) (< (+ index 1) limity)) (- (+ index 1) (getX nodo)) (getY nodo)))
          (z (if (>= (+ index 1) limity) (- (+ index 2) limity) (getZ nodo)))
          (alpha (getAlpha nodo))
          (beta (getBeta nodo))
         ]
    (newChild x y z alpha beta (not (getType nodo)))
  )
)
;Genera todos los hijos de un nodo
(define (createChildren nodo)
  (define (_createChilds nodo index)
    (if (< index 1) '()
        (cons (nextChild nodo index) (_createChilds nodo (- index 1)))
    )
  )
  (reverse(_createChilds nodo (getChildCount nodo)))
)

;Devuelve si un hijo es o no un nodo hoja
(define (isLeaf nodo)
  (> 1 (getChildCount nodo))
)

;Actualiza un nodo hijo, pone en su valor quien gana
(define (update-leaf nodo metadata)
  (define (value nodo) (if (getType nodo) -1 1))
  (list
   (setBest (setW nodo (value nodo)) (getId nodo))
   (addLeaf metadata 1)
))

;Actualiza un nodo, su alpha o beta según corresponda, su peso y cambia su id por la de su mejor hijo
(define (update root-node child metadata)
  (define (update-max root-node child);TOMAR MAYOR PESO
    (let* [
           (action (or(> (getAlpha root-node) (getW child))
                      (and(= (getAlpha root-node) (getW child)) (< (getChildCount child) (getChildCount (bestChild (getBest root-node)))))))
           (val  (if action (getAlpha root-node) (getW child)))
           (best (if action (getBest root-node) (getId child)))
           (other (getBeta child))
          ]
      (setBest (setBeta (setAlpha (setW root-node val) val) other) best)
  ))
  (define (update-min root-node child);TOMAR MENOR PESO
    (let* [
           (action (or(< (getBeta root-node) (getW child))
                      (and(= (getBeta root-node) (getW child)) (< (getChildCount child) (getChildCount (bestChild (getBest root-node)))))))
           (val  (if action (getBeta root-node) (getW child)))
           (best (if action (getBest root-node) (getId child)))
           (other (getAlpha child))
          ]
      (setBest (setAlpha (setBeta (setW root-node val) val) other) best)
  ))
  (list
   (if (getType root-node)
        (update-max root-node child)
        (update-min root-node child)
   )
   (addEvaluated metadata 1)
))

;COUNT 1
(define (count1 node)
  (+ (if (= 1 (getX node)) 1 0) (if (= 1 (getY node)) 1 0) (if (= 1 (getZ node)) 1 0))
)
;SUMA PAR
(define (sumEven nodo)
  (even? (getChildCount nodo))
)
;SUMA IMPAR
(define (sumOdd nodo)
  (odd? (getChildCount nodo))
)
;SUMA IMPAR FUTURA
(define (futureSumOdd nodo)
  (and (odd? (getChildCount (rootChild (getX nodo) (getY nodo) 1))) (odd? (getChildCount (rootChild (getX nodo) (getZ nodo) 1))) (odd? (getChildCount (rootChild (getZ nodo) (getY nodo) 1))))
)
;SUMA PAR FUTURA
(define (futureSumEven nodo)
  (and (even? (getChildCount (rootChild (getX nodo) (getY nodo) 1))) (even? (getChildCount (rootChild (getX nodo) (getZ nodo) 1))) (even? (getChildCount (rootChild (getZ nodo) (getY nodo) 1))))
)

(define (infere-leaf node metadata)
  (let* [
         (count (count1 node))
         (result (cond
                   [(and (= 2 count) (getType node)) #t]
                   [(and (= 1 count) (getType node) (sumOdd node)) #t]
                   [(and (= 1 count) (not(getType node)) (sumEven node)) #t]
                   [else #f]
                  ))
         (finalNode (if result (winnerChild (getId node)) (looserChild (getId node))))
        ]
  (list finalNode (addInfered metadata 1))
))

;MUL NODE
(define (mul-node node)
  (* (getX node) (getY node) (getZ node))
)
;LOGARITMO EN BASE 2
(define (log2 n)
  (/ (log n) (log 2))
)
;MAX
(define (max x y)
  (if (> x y) x y)
)

;MINIMAX
(define (minmax nodo lazzy infer)
  (define (cuter root-node) #f)
  (if lazzy
      (optimize nodo cuter infer)
      (optimize-not-lazzy nodo cuter infer)
))
;ALPHABETA
(define (alphabeta nodo lazzy infer)
  (define (cuter root-node) (> (getAlpha root-node) (getBeta root-node)))
  (if lazzy
      (optimize nodo cuter infer)
      (optimize-not-lazzy nodo cuter infer)
))
(define (inferable node)
  (and (<= (getX node) 3) (<= (getY node) 3) (<= (getZ node) 3))
)
;OPTIMIZADOR CON LAZZY
(define (optimize nodo check infer)
  (define (profundidad node metadata depth)    
    (if (isLeaf node)
        (update-leaf node (setDepth metadata depth))
        (if (and infer (inferable node) (> depth 2))
            (infere-leaf node (setDepth metadata depth))
            (anchura node node 1 (addCreated metadata (getChildCount node)) depth)
        )
   ))
  (define  (anchura original root-node expansion metadata depth)
    (if (> expansion (getChildCount original)) (list root-node metadata)
        (if (check root-node) (list root-node (addCut metadata 1))
            (let* [
                   (result1 (profundidad (nextChild original expansion) metadata (+ 1 depth)))
                   (evaluatedNode (car result1))
                   (newMetadata1 (cadr result1))
                   (result2 (update root-node evaluatedNode newMetadata1))
                   (updated (car result2))
                   (newMetadata2 (cadr result2))
                  ]
              (anchura original updated (+ expansion 1) (addExpanded newMetadata2 1) depth)
    ))))
  (time (let* [
         (result (profundidad nodo (newMetadata) 0))
         (action (car result))
         (metadata (cadr result))
        ]
    (printMetadata metadata)
    (bestChild (getBest action))
)))

;OPTIMIZADOR SIN LAZZY
(define (optimize-not-lazzy nodo check infer)
  (define (profundidad node metadata depth)    
    (if (isLeaf node)
        (update-leaf node (setDepth metadata depth))
        (if (and infer (inferable node) (> depth 2))
            (infere-leaf node (setDepth metadata depth))
            (anchura node (createChildren node) (addCreated metadata (getChildCount node)) depth)
        )
   ))
  (define  (anchura root-node child-list metadata depth)
    (if (null? child-list) (list root-node metadata)
        (if (check root-node) (list root-node (addCut metadata 1))
            (let* [
                   (result1 (profundidad (car child-list) metadata (+ 1 depth)))
                   (evaluatedNode (car result1))
                   (newMetadata1 (cadr result1))
                   (result2 (update root-node evaluatedNode newMetadata1))
                   (updated (car result2))
                   (newMetadata2 (cadr result2))
                  ]
              (anchura updated (cdr child-list) (addExpanded newMetadata2 1) depth)
    ))))
  (time (let* [
         (result (profundidad nodo (newMetadata) 0))
         (action (car result))
         (metadata (cadr result))
        ]
    (printMetadata metadata)
    (bestChild (getBest action))
)))





