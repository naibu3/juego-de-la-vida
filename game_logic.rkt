#lang racket

#|
Variables de configuración del juego:
    
    - min-survive: Valor mínimo de vecinos para que una célula sobreviva (entero)
    - max-survive: Valor máximo de vecinos para que una célula sobreviva (entero)
    - birth-neighbours: Número exacto de vecinos necesarios para que una célula muerta cobre vida (entero).
|#
(define min-survive 2)
(define max-survive 3)
(define birth-neighbours 3)

#|
Interfaz del módulo
|#
(provide update-grid get-neighbours life-rule map-indexed coords->index index->coords
         set-rules min-survive max-survive birth-neighbours import-board export-board)

#| set-rules
Objetivo: Establece nuevas reglas para el juego, especificando los valores de supervivencia mínima,
          supervivencia máxima y número de vecinos necesarios para el nacimiento de una célula.
Parámetros:
    - new-min: Nuevo valor mínimo de vecinos para que una célula sobreviva (entero).
    - new-max: Nuevo valor máximo de vecinos para que una célula sobreviva (entero).
    - new-birth: Número exacto de vecinos necesarios para que una célula muerta cobre vida (entero).
Return: No retorna ningún valor (modifica las reglas globales).
|#
(define (set-rules new-min new-max new-birth)
  (set! min-survive new-min)
  (set! max-survive new-max)
  (set! birth-neighbours new-birth))

#| coords->index
Objetivo: Convierte coordenadas (x, y) a un índice unidimensional.
Parámetros:
    - x: Coordenada horizontal (entero).
    - y: Coordenada vertical (entero).
    - width: Ancho de la cuadrícula (entero).
Return: Un índice unidimensional que corresponde a las coordenadas dadas.
|#
(define (coords->index x y width)
  (+ x (* y width)) )

#| index->coords
Objetivo: Convierte un índice unidimensional a coordenadas (x, y).
Parámetros:
    - index: Índice unidimensional (entero).
    - width: Ancho de la cuadrícula (entero).
Return: Una lista `(x y)` con las coordenadas correspondientes al índice.
|#
(define (index->coords index width)
  (list (modulo index width) (quotient index width)) )

#| get-neighbours
Objetivo: Obtener los vecinos válidos de una celda específica en la cuadrícula.
Parámetros:
    - index: Índice de la celda actual (entero).
    - width: Ancho de la cuadrícula (entero).
    - height: Alto de la cuadrícula (entero).
Return: Una lista de índices unidimensionales que representan las celdas vecinas.
|#
(define (get-neighbours index width height)

  ;; Funcion interna que determina si unas coordenadas están dentro de rango.
  (define (valid? x y)
    (and (>= x 0) (< x width)
         (>= y 0) (< y height) )
    )
  
  (define (neighbour-coords x y)
    (filter (lambda (coords) (apply valid? coords))
            (list (list (- x 1) (- y 1)) (list x (- y 1)) (list (+ x 1) (- y 1))
                  (list (- x 1) y)                     (list (+ x 1) y)
                  (list (- x 1) (+ y 1)) (list x (+ y 1)) (list (+ x 1) (+ y 1)))))
  (let ((coords (index->coords index width)))
    (map (lambda (coord) (coords->index (car coord) (cadr coord) width))
         (neighbour-coords (car coords) (cadr coords)))))

#| life-rule
Objetivo: Regla que determina si una celda debe estar viva o muerta en la próxima generación.
Parámetros:
    - alive: Estado actual de la celda (booleano).
    - neighbours: Número de vecinos vivos (entero).
Return: Un booleano indicando si la celda estará viva (true) o muerta (false).
|#
(define (life-rule cell neighbours)
  (let ((alive-neighbours (count (lambda (n) (= n 1)) neighbours)))
    (cond
      ((and (= cell 1) (>= alive-neighbours min-survive) (<= alive-neighbours max-survive)) 1)
      ((and (= cell 0) (= alive-neighbours birth-neighbours)) 1)
      (else 0))))



#|
Objetivo: Aplica una función a cada elemento de una lista, incluyendo su índice.
Parámetros:
    - proc: Una función que toma dos argumentos: el índice del elemento y el elemento en sí.
    - lst: Una lista cuyos elementos serán procesados.
Return: Una nueva lista con los resultados de aplicar `proc` a cada elemento junto con su índice.
|#
(define (map-indexed proc lst)
  (define (loop lst index)
    (if (null? lst)
        '()
        (cons (proc index (car lst))
              (loop (cdr lst) (+ index 1)))))
  (loop lst 0))

#|
Objetivo: Actualizar el estado de una cuadrícula aplicando una regla a cada célula, basada en su estado actual y el estado de sus vecinos.
Parámetros:
    - cells: Lista que representa el estado actual de la cuadrícula (booleanos o valores personalizados para cada célula).
    - width: Ancho de la cuadrícula (entero).
    - height: Alto de la cuadrícula (entero).
    - rule: Función que toma el estado actual de una célula y una lista de estados de sus vecinos, y retorna el nuevo estado de la célula.
Return: Una nueva lista que representa el estado actualizado de la cuadrícula.
|#
(define (update-grid cells width height rule)
  (define neighbourhoods
    (lambda (cell)
      (get-neighbours cell width height)))
  (map-indexed
   (lambda (index cell)
     (rule cell (map (lambda (idx) (list-ref cells idx))
                     (neighbourhoods index))))
   cells))

#| import-board
Objetivo: Importar un tablero desde un archivo de texto que contiene una representación en formato de matriz,
          donde cada fila está separada por saltos de línea y los valores de las celdas están separados por espacios.

Parámetros:
    - filename: Nombre del archivo (cadena de texto) que contiene el tablero a importar.

Formato del archivo:
    Cada línea representa una fila del tablero, y los valores (0 o 1) están separados por espacios. Ejemplo:
        0 1 0 0
        1 0 1 0
        0 0 0 1
        1 1 1 0

Return: Una lista plana de números (0 y 1) que representa el tablero importado.
|#
(define (import-board filename)
  (with-input-from-file filename
    (lambda ()
      (let (; Variables de let
            [lines (port->lines (current-input-port))]
            )
        ; Cuerpo de let
        (apply append
               (map (lambda (line)
                      (map string->number (string-split line))
                      )
                    lines)
               )
        )
      )
    )
  )

#|
Objetivo: Exportar el estado de un tablero a un archivo en formato de matriz, donde cada fila se escribe como una línea y los valores de las celdas están separados por espacios.
Parámetros:
    - filename: Nombre del archivo (cadena de texto) donde se exportará el tablero.
    - board: Lista plana que representa el tablero (números 0 o 1).
    - width: Ancho del tablero (entero).
    - height: Alto del tablero (entero).
Formato del archivo:
    Cada línea representa una fila del tablero, y los valores están separados por espacios. Ejemplo:
        0 1 0 0
        1 0 1 0
        0 0 0 1
        1 1 1 0
Return: No retorna ningún valor (genera un archivo con el tablero exportado).
|#
(define (export-board filename board width height)
  (with-output-to-file filename
    (lambda ()
      ;; Escribe el tablero en formato de líneas
      (for (; Variables de for
            [y (in-range height)]
            )
        ; Cuerpo de for
        (let (; Variables de let
              [line (for/list ([x (in-range width)])
                      (number->string (list-ref board (+ x (* y width)))) )]
              )
          ; Cuerpo de let
          (displayln (string-join line " "))
          )
        )
      )
    )
  )

