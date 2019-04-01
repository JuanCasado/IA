#lang racket

(require graph)
(require racket/gui/base)

(define ciudades '(
               ("Coruña" ("Vigo" 171) ("Valladolid" 455)) 
               ("Vigo" ("Coruña" 171) ("Valladolid" 365)) 
               ("Oviedo" ("Bilbao" 304)) 
               ("Valladolid" ("Coruña" 455) ("Vigo" 365) ("Bilbao" 280) ("Madrid" 193)) 
               ("Bilbao" ("Oviedo" 304) ("Valladolid" 280) ("Zaragoza"324) ("Madrid" 395)) 
               ("Zaragoza" ("Bilbao" 324) ("Madrid" 325) ("Barcelona" 296)) 
               ("Madrid" ("Valladolid" 193) ("Bilbao" 395) ("Zaragoza" 325) ("Barcelona" 296)) 
               ("Badajoz" ("Madrid" 403)) 
               ("Barcelona" ("Zaragoza" 403) ("Gerona" 100) ("Valencia" 349)) 
               ("Gerona" ("Barcelona" 100)) 
               ("Valencia" ("Barcelona"349) ("Murcia" 241) ("Albacete" 191)) 
               ("Murcia" ("Valencia" 241) ("Albacete" 150) ("Granada" 278)) 
               ("Albacete" ("Madrid" 251) ("Valencia" 241) ("Murcia" 150)) 
               ("Jaén" ("Madrid" 355) ("Granada" 99) ("Sevilla" 242)) 
               ("Granada" ("Murcia" 278) ("Jaén" 99) ("Sevilla" 256)) 
               ("Sevilla" ("Jaén" 242) ("Granada" 256) ("Cádiz" 125)) 
               ("Cádiz" ("Sevilla" 125)) 
              )
  )
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
    [(member (car ciudades) path) (hash-set! ht (car ciudades) 1)(cons (list (car ciudades) 1) (setColors (cdr ciudades) path))]
    [else (hash-set! ht (car ciudades) 2) (cons (list (car ciudades) 2) (setColors (cdr ciudades) path))]
  )
)

(define (cityColors ciudades path)
   (setColors (ciudadesList ciudades) path)
)
(define (colorar g)
  ht
)

(define dot-command "/usr/local/bin/dot -Tpng ~a >~a")

;; Given a graph, use dot to create a png and return the path to the png
(define (make-graphviz-png g)
  (cityColors ciudades '("Zaragoza"))
  (let ([dot-string (graphviz g #:colors (colorar g))]
        [dot-file (make-temporary-file "example~a.dot")]
        [png-file (make-temporary-file "example~a.png")])
    (write (coloring/brelaz g))
    (display-to-file dot-string dot-file #:exists 'replace)
    (system (format dot-command dot-file png-file))
    png-file))

;; The graph
(define test-graph (weighted-graph/directed (formatCiudades ciudades)))

;; Generate the png from the graph
(define bitmap (read-bitmap (make-graphviz-png test-graph)))

;; Display in a frame
(define f (new frame% [label "Your Graph"]))

(define (show)
   (new message% [parent f] [label bitmap])
   (send f show #t)                  
)