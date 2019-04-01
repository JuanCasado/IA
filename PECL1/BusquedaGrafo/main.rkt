#lang racket

(require "grafo.rkt")                ;Ejemplo de datos de entrada
(require "busquedaGrafos.rkt")       ;Funciones generales para realizar la búsqueda
(require "busquedaAnchura.rkt")      ;Funciones exclusivas de la búsqueda en Anchura
(require "busquedaProfundidad.rkt")  ;Funciones exclusivas de la búsqueda en Profundidad
(require "busquedaPrimero.rkt")      ;Funciones exclusivas de la búsqueda Optimal/Primero el mejor
(require "gra.rkt")                  ;Para pintar el grado

;Interfaz que permite pintar el grafo
(define (pintaBusqueda objetivo ciudades tipo_busqueda)
  (let ((path (inicioBusqueda objetivo ciudades tipo_busqueda)))
  (show (nextNumber) ciudades path)
    path
  )
)

;Interfaz que prepara los datos iniciales del algoritmo de búsqueda elegido
(define (inicioBusqueda objetivo ciudades tipo_busqueda)
  (time(cond
    [(string=? tipo_busqueda "anchura")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_anchura siguientes abiertos)))]
    [(string=? tipo_busqueda "profundidad")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_profundidad siguientes abiertos)))]
    [(string=? tipo_busqueda "primero")(busquedaInicio objetivo ciudades
               (lambda (siguientes abiertos) (insertar_siguientes_primero siguientes abiertos)))]
  ))
)

;Algoritmo de Búsqueda basado en ACTUAL SIGUENTES ABIERTOS CERRADOS
(define (busquedaInicio objetivo ciudades algoritmo_insercion)
  (define (busqueda objetivo ciudades abiertos cerrados algoritmo_insercion)
    (if (null? abiertos) '() ;No hay camino
      (let* ((actual (obtenerPrimeroAbiertos abiertos))) ;Se obtine el actual
        (if
         (ha_finalizado objetivo actual) actual ;Si el actual es el objetivo terminamos
         ;Obtenemos los siguientes sin los caminos que llevan a un nodo cerrado y con el actual añadido a cada camino
         (let* ((siguientes (aumentarCaminos (eliminarCerrados (obtenerSiguientes (obtenerUltimo actual) ciudades) cerrados) actual))
                (abiertos (eliminarPrimeroAbiertos abiertos)) ;Obtenemos la lista de abiertos eliminando el elemento actual
                (abiertos (algoritmo_insercion siguientes abiertos));Cambiando esta funcion Lambda el algoritmo cambia de tipo de búsqueda
                (cerrados (cons (obtenerUltimo actual) cerrados)));Actualizamos la lista de cerrados
            (busqueda objetivo ciudades abiertos cerrados algoritmo_insercion))
         )
        )
      )
    )
  (busqueda objetivo ciudades (list(list (car objetivo) 0)) '() algoritmo_insercion);Primera llamada al algoritmo
)
