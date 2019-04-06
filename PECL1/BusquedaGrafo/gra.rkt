#lang racket

(require graph)
(require racket/gui/base)
(provide (all-defined-out))

;Proporciona un número nuevo cada vez con el que identificar las ventanas creadas
(define (numberIncrementer)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))
(define nextNumber (numberIncrementer))

;Transforma el archivo .dot a una imagen
(define dot-command
  (let ((os (system-type 'os)))
    (cond
      [(equal? os 'windows) "dot -Tpng ~a >~a""dot -Tpng ~a >~a"]
      [else "/usr/local/bin/dot -Tpng ~a >~a"]
    )
  )
)

;Tabla para almacenar los colores del grafo
(define ht (make-hash))

;Modificación del formato del grafo
(define (formatCiudades ciudades)
  (define (formatCiudad ciudad)
    (define (linkCiudad ciudad ciudades)
      (cond
        [(null? ciudades) '()]
        [else (cons (reverse (cons (caar ciudades) (cons ciudad (cdar ciudades)))) (linkCiudad ciudad (cdr ciudades)))]
      )
    )
    (linkCiudad (car ciudad) (cdr ciudad))
  )
  (cond
    [(null? ciudades) '()]
    [else (append (formatCiudad (car ciudades)) (formatCiudades (cdr ciudades)))]
  )
)
;Modificación del camino y creación de la tabla de colores
(define (cityColors ciudades path)
  (define (esCamino ciudad camino)
    (cond
      [(null? camino) #f]
      [(number? (car camino)) #f]
      [(string=? (car camino) ciudad) #t]
      [else (esCamino ciudad (cdr camino))]
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
       [(esCamino (car ciudades) path) (hash-set! ht (car ciudades) 1)(cons (list (car ciudades) 1) (setColors (cdr ciudades) path))]
       [else (hash-set! ht (car ciudades) 3) (cons (list (car ciudades) 3) (setColors (cdr ciudades) path))]
     )
   )
   (setColors (ciudadesList ciudades) path)
)

;Dado un grago de búsqueda este es tranformado al formato esperado por la librería, posteriormente este se procesa para crear una imagen
(define (make-graphviz-png g c p)
  (cityColors c p)
  ;Creación del archivo dot
  (let ([dot-string (graphviz g #:colors ht)]
        [dot-file (make-temporary-file "./temporal~a.dot")]
        [png-file (make-temporary-file "./temporal~a.png")])
    (display-to-file dot-string dot-file #:exists 'replace)
    (system (format dot-command dot-file png-file))
    png-file))
; Crea el grafo
(define (grafo ciudades) (weighted-graph/directed (formatCiudades ciudades)))
; Crea una imagen a partir del grafo
(define (bitmap ciudades path) (read-bitmap (make-graphviz-png (grafo ciudades) ciudades path)))
; Muestra el grafo
(define (f n) (new frame% [label (~a "Busqueda" n)]))

(define (show number ciudades path)
  (let ((display (f number)))
   (new message% [parent display] [label (bitmap ciudades path)])
   (send display show #t)
  )
)


