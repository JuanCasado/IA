#lang racket

(require "grafo.rkt")
(require "busquedaGrafos.rkt")
(require "busquedaAnchura.rkt")

(define (inicioBusqueda objetivo ciudades tipo_busqueda)
  (cond
    [(string=? tipo_busqueda "anchura")(busquedaAnchuraInicio objetivo ciudades)]
    [(string=? tipo_busqueda "profundidad")(write "busqueda anchura")]
    [(string=? tipo_busqueda "primero_mejor")(write "busqueda anchura")]
  )
)


(define (busquedaAnchuraInicio objetivo ciudades)
  (define (busquedaAnchura objetivo ciudades actual abiertos cerrados)
    (cond
      [(ha_finalizado objetivo actual) actual]
      
      [else (let* ((siguientes (eliminarCerrados (obtenerSiguientes (obtenerUltimo actual) ciudades) cerrados))
                   (abiertos (insertar_siguientes_anchura siguientes abiertos))
                   (primero (obtenerPrimeroAbiertos abiertos))
                   (actual (aumentarCamino actual primero))
                   (cerrados (cons (obtenerUltimo actual) cerrados)))
              (busquedaAnchura objetivo ciudades actual (eliminarPrimeroAbiertos abiertos) cerrados))]
    )
  )
  (busquedaAnchura objetivo ciudades (list (car objetivo) 0) '() (list (car objetivo)))
)
(eliminarCerrados '(("Oviedo" "Bilbao" "Oviedo" "Valladolid" "Zaragoza" "Madrid" 1607) ("Bilbao" "Oviedo" "Madrid" "Valladolid" "Oviedo" 233)) '("Madrid" "Oviedo"))
(inicioBusqueda '("Oviedo" "Valencia") ciudades "anchura")