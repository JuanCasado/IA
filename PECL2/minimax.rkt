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
(define (update-leaf nodo)
  (define (value nodo) (if (getType nodo) -1 1))
  (addLeaf 1)
  (setBest (setW nodo (value nodo)) (getId nodo))   
)

;Actualiza un nodo, su alpha o beta según corresponda, su peso y cambia su id por la de su mejor hijo
(define (update root-node child)
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
  (addEvaluated 1)
  (if (getType root-node)
        (update-max root-node child)
        (update-min root-node child)
  )   
)

;INFIERE EL VALOR DE UN NODO INFERIOR A (3 3 3)
(define (infere-leaf node)
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
    (addInfered 1)
    finalNode
))

(define ht (make-hash))
;MINIMAX
(define (minmax nodo lazzy infer cache)
  (define (cuter root-node) #f)
  (newMetadata)
  (hash-clear! ht)
  (if lazzy
      (optimize nodo cuter infer cache)
      (optimize-not-lazzy nodo cuter infer cache)
))
;ALPHABETA
(define (alphabeta nodo lazzy infer cache)  
  (define (cuter root-node) (> (getAlpha root-node) (getBeta root-node)))
  (newMetadata)
  (hash-clear! ht)
  (if lazzy
      (optimize nodo cuter infer cache)
      (optimize-not-lazzy nodo cuter infer cache)
))
(define (inferable node)
  (and (<= (getX node) 3) (<= (getY node) 3) (<= (getZ node) 3))
)

;OPTIMIZADOR CON LAZZY
(define (optimize nodo check infer cache)
  (define (profundidad node depth)    
    (setDepth depth)
    (if (isLeaf node)        
        (update-leaf node)
        (if (and infer (inferable node) (> depth 2))
            (infere-leaf node)
            (if cache 
                (if (hash-has-key? ht node)
                    (let [] (addHit 1) (hash-ref ht node))
                    (let [(result(anchura node node 1 depth))]
                      (hash-set! ht node result)
                      (addCreated (getChildCount node))
                      result
                 ))
            (let [] (addCreated (getChildCount node)) (anchura node node 1 depth))
       ))
   ))
  (define  (anchura original root-node expansion depth)
    (if (> expansion (getChildCount original)) root-node
        (if (check root-node)
            (let [] (addCut 1) root-node)
            (let [] (addExpanded 1) (anchura original (update root-node (profundidad (nextChild original expansion) (+ 1 depth))) (+ expansion 1) depth))
    )))
  (time
    (let [(best (bestChild (getBest (profundidad nodo 0))))] (printMetadata) best)
))

;OPTIMIZADOR SIN LAZZY
(define (optimize-not-lazzy nodo check infer cache)
  (define (profundidad node depth)    
    (setDepth depth)
    (if (isLeaf node)        
        (update-leaf node)
        (if (and infer (inferable node) (> depth 2))
            (infere-leaf node)
            (if cache 
                (if (hash-has-key? ht node)
                    (let [] (addHit 1) (hash-ref ht node))
                    (let [(result(anchura node (createChildren node) depth))]
                      (addCreated (getChildCount node)) (hash-set! ht node result)
                      result
                 ))
            (let [] (addCreated (getChildCount node)) (anchura node (createChildren node) depth))
       ))
   ))
  (define  (anchura root-node child-list depth)
    (if (null? child-list) root-node
        (if (check root-node)
            (let [] (addCut 1) root-node)
            (let [] (addExpanded 1)  (anchura (update root-node (profundidad (car child-list) (+ 1 depth))) (cdr child-list) depth))
    )))
  (time
    (let [(best (bestChild (getBest (profundidad nodo 0))))] (printMetadata) best)
))





