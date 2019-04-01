#lang racket

(require "grafo.rkt")
(require "busquedaGrafos.rkt")
(require "busquedaAnchura.rkt")
(require "busquedaProfundidad.rkt")
(require "busquedaPrimero.rkt")
(require "gra.rkt")

(define (pintaBusqueda objetivo ciudades tipo_busqueda)
  (let ((path (inicioBusqueda objetivo ciudades tipo_busqueda)))
  (show (nextNumber) ciudades path)
    path
  )
)

(define (inicioBusqueda objetivo ciudades tipo_busqueda)
  (cond
    [(string=? tipo_busqueda "anchura")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_anchura siguientes abiertos)))]
    [(string=? tipo_busqueda "profundidad")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_profundidad siguientes abiertos)))]
    [(string=? tipo_busqueda "primero")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_primero siguientes abiertos)))]
  )
)

(define (busquedaInicio objetivo ciudades algoritmo_insercion)
  (define (busqueda objetivo ciudades abiertos cerrados algoritmo_insercion)
    (let* ((actual (obtenerPrimeroAbiertos abiertos)))
      (if
         (ha_finalizado objetivo actual) actual
         (let* ((siguientes (aumentarCaminos (eliminarCerrados (obtenerSiguientes (obtenerUltimo actual) ciudades) cerrados) actual))
                (abiertos (eliminarPrimeroAbiertos abiertos))
                (abiertos (algoritmo_insercion siguientes abiertos))
                (cerrados (cons (obtenerUltimo actual) cerrados)))
            (busqueda objetivo ciudades abiertos cerrados algoritmo_insercion))
         )
      )
    )
  (busqueda objetivo ciudades (list(list (car objetivo) 0)) '() algoritmo_insercion)
)
