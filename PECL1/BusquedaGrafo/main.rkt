#lang racket

(require "grafo.rkt")
(require "busquedaGrafos.rkt")
(require "busquedaAnchura.rkt")
(require "busquedaProfundidad.rkt")

(define (inicioBusqueda objetivo ciudades tipo_busqueda)
  (cond
    [(string=? tipo_busqueda "anchura")(busquedaAnchuraInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_profundidad siguientes abiertos)))]
    [(string=? tipo_busqueda "profundidad")(write "busqueda anchura")]
    [(string=? tipo_busqueda "primero_mejor")(write "busqueda anchura")]
  )
)

(define (busquedaAnchuraInicio objetivo ciudades algoritmo_busqueda)
  (define (busquedaAnchura objetivo ciudades abiertos cerrados algoritmo_busqueda)
    (let* ((actual (obtenerPrimeroAbiertos abiertos)))
      (if
         (ha_finalizado objetivo actual) actual
         (let* ((siguientes (aumentarCaminos (eliminarCerrados (obtenerSiguientes (obtenerUltimo actual) ciudades) cerrados) actual))
                (abiertos (eliminarPrimeroAbiertos abiertos))
                (abiertos (algoritmo_busqueda siguientes abiertos))
                (cerrados (cons (obtenerUltimo actual) cerrados)))
            (busquedaAnchura objetivo ciudades abiertos cerrados) algoritmo_busqueda)
         )
      )
    )
  (busquedaAnchura objetivo ciudades (list(list (car objetivo) 0)) '() algoritmo_busqueda)
)

;(inicioBusqueda '("Oviedo" "Valencia") ciudades "anchura")