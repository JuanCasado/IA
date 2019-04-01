#lang racket

(require graph)
(require racket/gui/base)

(provide (all-defined-out))

(define ht (make-hash))

(define (linkCiudad ciudad ciudades)
  (cond
    [(null? ciudades) '()]
    [else (cons (reverse(cons ciudad (car ciudades))) (linkCiudad ciudad (cdr ciudades)))]
  )
)

(define (formatCiudad ciudad)
  (linkCiudad (car ciudad) (cdr ciudad))
)

(define (formatCiudades ciudades)
  (cond
    [(null? ciudades) '()]
    [else (append (formatCiudad (car ciudades)) (formatCiudades (cdr ciudades)))]
  )
)

(define (ciudadesList ciudades)
  (cond
    [(null? ciudades) '()]
    [else (cons (caar ciudades) (ciudadesList (cdr ciudades)))]
  )
)

(define (setColors ciudades path)
  (cond
    [(null? ciudades) ]
    [(esCamino (car ciudades) path) (hash-set! ht (car ciudades) 3)(cons (list (car ciudades) 3) (setColors (cdr ciudades) path))]
    [else (hash-set! ht (car ciudades) 1) (cons (list (car ciudades) 1) (setColors (cdr ciudades) path))]
  )
)

(define (cityColors ciudades path)
   (setColors (ciudadesList ciudades) path)
)

(define (numberIncrementer)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))
(define nextNumber (numberIncrementer))

(define (esCamino ciudad camino)
  (cond
    [(null? camino) #f]
    [(number? (car camino)) #f]
    [(string=? (car camino) ciudad) #t]
    [else (esCamino ciudad (cdr camino))]
  )
)

(define dot-command "/usr/local/bin/dot -Tpng ~a >~a")

;; Given a graph, use dot to create a png and return the path to the png
(define (make-graphviz-png g c p)  
  (cityColors c p)
  (let ([dot-string (graphviz g #:colors ht)]
        [dot-file (make-temporary-file "example~a.dot")]
        [png-file (make-temporary-file "example~a.png")])
    ;(write (coloring/brelaz g))
    (display-to-file dot-string dot-file #:exists 'replace)
    (system (format dot-command dot-file png-file))
    png-file))

;; The graph
(define (grafo ciudades) (weighted-graph/directed (formatCiudades ciudades)))

;; Generate the png from the graph
(define (bitmap ciudades path) (read-bitmap (make-graphviz-png (grafo ciudades) ciudades path)))

;; Display in a frame
(define (f n) (new frame% [label (~a "Your Graph" n)]))

(define (show number ciudades path)
  (let ((display (f number)))
   (new message% [parent display] [label (bitmap ciudades path)])
   (send display show #t)
  )
)


