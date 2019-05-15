#lang racket

(provide (all-defined-out))

;CREATED EVALUATED INFERED LEAF CUT EXPANDED DEPTH
(define (newMetadata)
  '(0 0 0 0 0 0 0)
)

(define (addEvaluated metadata value)
  (cons (car metadata) (cons (+ (cadr metadata)value) (cddr metadata)))
)

(define (addCreated metadata value)
  (cons (+ (car metadata)value) (cdr metadata))
)

(define (addInfered metadata value)
  (list (car metadata) (cadr metadata) (+ (caddr metadata) value) (cadddr metadata) (caddr (reverse metadata)) (cadr (reverse metadata)) (car (reverse metadata)))
)

(define (addLeaf metadata value)
  (list (car metadata) (cadr metadata) (caddr metadata) (+ (cadddr metadata)  value) (caddr (reverse metadata)) (cadr (reverse metadata)) (car (reverse metadata)))
)

(define (addExpanded metadata value)
  (list (car metadata) (cadr metadata) (caddr metadata) (cadddr metadata) (caddr (reverse metadata)) (+ (cadr (reverse metadata))  value) (car (reverse metadata)))
)

(define (addCut metadata value)
  (list (car metadata) (cadr metadata) (caddr metadata) (cadddr metadata) (+ (caddr (reverse metadata))  value) (cadr (reverse metadata)) (car (reverse metadata)))
)

(define (setDepth metadata depth)
  (if (> depth (car (reverse metadata)))
     (list (car metadata) (cadr metadata) (caddr metadata) (cadddr metadata) (caddr (reverse metadata)) (cadr (reverse metadata)) depth)
      metadata)
)

(define (printMetadata metadata)  
  (display "\nCREATED:       ")
  (display (cadr (reverse metadata)))
  (display "\nLAZZY SAVINGS: ")
  (display (- (car metadata) (cadr (reverse metadata))))
  (display "\nEVALUATED:     ")
  (display (cadr metadata))
  (display "\nCUTS:          ")
  (display (caddr (reverse metadata)))
  (display "\nINFERED:       ")
  (display (caddr metadata))
  (display "\nLEAF:          ")
  (display (cadddr metadata))
  (display "\nDEPTH          ")
  (display (car (reverse metadata)))
  (display "\n")
)