#lang racket
;Manejador de archivos
;(string->number (caddr list))
(provide (all-defined-out))
(define (leerArchivo archivo)
  ;Leer el grafo de disco
  (define (read-graph file)
    (let ([line (read-line file 'any)])
      (if (eof-object? line)
          empty
          (cons (crea-nodo-grafo line) (read-graph file))
      )
    )
  )
  ;Crea una lista con el contenido del archivo
  (define (crea-nodo-grafo line)
    (let ([list (string-split line)])
        list
    )
  )
  ;Transforma la lista leida de disco en el formato que usamos
  (define (transformarEntrada lista listaFinal)
    ;Devuelve si la ciudad estudiada ya ha sido insertada en la listaFinal
    (define (estaEnLista ciudad lista)
      (cond
        [(null? lista) #false]
        [(string=? ciudad (caar lista)) #true]
        [else (estaEnLista ciudad (cdr lista))]
      )
    )
    ;Inserta la ciudad destino de una ciudad origen que ya estaba en listaFinal
    (define (insertarExistente objetivo ciudad listaFinal)
      (cond
        [(string=? objetivo (caar listaFinal)) (cons (append (list (caar listaFinal) ciudad) (cdar listaFinal)) (cdr listaFinal))]
        [else (cons (car listaFinal) (insertarExistente objetivo ciudad (cdr listaFinal)))]
      ) 
    )
    ;Inserta una nueva ciudad origen en listaFinal
    (define (insertarNoExistente ciudad listaFinal)
      (append listaFinal (list (list (car ciudad) (formatearCiudad ciudad))))
    )
    
    ;Transformamos un nodo de tal forma que nos quedemos con la
    ;ciudad a la que podemos ir y su distancia -FUNCIONA
    (define (formatearCiudad ciudad)
      (list (car ciudad) (string->number (car (reverse ciudad))))
    )
    ;(insertarExistente "Oviedo" '("Pepe" 304) '(("Oviedo" ("Bilbao" 304))))
    
    (cond
      [(null? lista) listaFinal]
      [(estaEnLista (caar lista) listaFinal) (transformarEntrada (cdr lista) (insertarExistente (caar lista) (formatearCiudad (cdar lista)) listaFinal))]
      [else (transformarEntrada (cdr lista) (insertarNoExistente (car lista) listaFinal))]
     )
    
    ;q(insertarNoExistente '("X" "D" "111") (insertarNoExistente '("A" "B" "555") '()))
    ;q(formatearCiudad '("Oviedo" "Bilbao" "304"))
  )
  (transformarEntrada (call-with-input-file archivo read-graph) '())
  
  
)
;(car (call-with-input-file "entrada.txt" read-graph))
(leerArchivo "entrada.txt")