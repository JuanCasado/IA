#lang racket

(provide (all-defined-out))

(define created "CREATED")
(define evaluated "EVALUATED")
(define infered "INFERED")
(define leaf "LEAF")
(define cache "CACHE")
(define cut "CUT")
(define expanded "EXPANDED")
(define depth "DEPTH")

(define metadata (make-hash))
(define (newMetadata)
  (hash-set! metadata created 0)
  (hash-set! metadata evaluated 0)
  (hash-set! metadata infered 0)
  (hash-set! metadata leaf 0)
  (hash-set! metadata cache 0)
  (hash-set! metadata cut 0)
  (hash-set! metadata expanded 0)
  (hash-set! metadata depth 0)
)

(define (adder name value)
  (hash-set! metadata name (+(hash-ref metadata name) value))
)

(define (addCreated value)
  (adder created value)
)

(define (addEvaluated value)
  (adder evaluated value)
)

(define (addInfered value)
  (adder infered value)
)

(define (addLeaf value)
  (adder leaf value)
)

(define (addHit value)
  (adder cache value)
)

(define (addCut value)
  (adder cut value)
)

(define (addExpanded value)
  (adder expanded value)
)

(define (setDepth value)
  (let* [
         (current (hash-ref metadata depth))
        ]
    (if (> value current)
        (adder depth value)
        ""
  ))
)

(define (printMetadata)  
  (display "\nCREATED:        ")
  (display (hash-ref metadata created))
  (display "\nLAZZY SAVINGS:  ")
  (display (- (hash-ref metadata created) (hash-ref metadata expanded)))
  (display "\nEVALUATED:      ")
  (display (hash-ref metadata evaluated))
  (display "\nCUTS:           ")
  (display (hash-ref metadata cut))
  (display "\nINFERED:        ")
  (display (hash-ref metadata infered))
  (display "\nCACHE HIT:      ")
  (display (hash-ref metadata cache))
  (display "\nLEAF:           ")
  (display (hash-ref metadata leaf))
  (display "\nDEPTH:          ")
  (display (hash-ref metadata depth))
)