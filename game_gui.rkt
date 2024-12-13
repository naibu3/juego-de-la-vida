#lang racket

(require racket/gui
         "game_logic.rkt"
         ;"sounds.rkt"
         )

(define default-width 100)
(define default-height 40)
(define cell-size 10)

(provide create-main-menu create-game-window default-width default-height)

(define (create-rules-window on-save)
  (define frame (new frame% [label "Ajustar Reglas"]))
  (define main-panel (new vertical-panel% [parent frame]))

  ;; Inputs para ajustar las reglas
  (define min-survive-input (new text-field% [label "Mínimo vecinos para sobrevivir:"]
                                 [parent main-panel]
                                 [init-value (number->string min-survive)]))

  (define max-survive-input (new text-field% [label "Máximo vecinos para sobrevivir:"]
                                 [parent main-panel]
                                 [init-value (number->string max-survive)]))

  (define birth-neighbours-input (new text-field% [label "Vecinos para nacer:"]
                                      [parent main-panel]
                                      [init-value (number->string birth-neighbours)]))

  ;; Botón para guardar las reglas
  (define save-rules-button (new button% [parent main-panel]
       [label "Guardar reglas"]
       [callback
        (lambda (button event)
          (let ([new-min (string->number (send min-survive-input get-value))]
                [new-max (string->number (send max-survive-input get-value))]
                [new-birth (string->number (send birth-neighbours-input get-value))])
            (set-rules new-min new-max new-birth)
            (send frame show #f)
            (on-save)))])
    ;; Agregar sonido al botón
    ;;(add-sound-to-button save-rules-button button-sound)
    )

  ;; Botón para volver al menú principal
  (new button% [parent main-panel]
       [label "Volver al Menú Principal"]
       [callback (lambda (button event)
                   (begin
                     (create-main-menu default-width default-height) ; Crear el menú principal
                     (send frame show #f)))]) ; Cerrar la ventana actual

  (send frame show #t))

#| draw-grid
Objetivo: Dibuja una cuadrícula en un canvas.

Parámetros:
- dc: Objeto de tipo `dc<%>`. Es el contexto gráfico donde se dibujará la cuadrícula.
- cells: Lista unidimensional que representa el estado de las celdas (0 para muerta, 1 para viva).
- width: Entero que indica el número de columnas de la cuadrícula.
- height: Entero que indica el número de filas de la cuadrícula.

Return: No hay retorno de  valores

Observaciones:
|#
(define (draw-grid dc cells width height)
  ;; Limpia el canvas
  (send dc clear)

  ;; Configura el pincel para las líneas de la cuadrícula
  (send dc set-pen (make-object pen% "light gray" 1 'solid))

  ;; Dibuja la cuadrícula
  (for* ([y (in-range height)] [x (in-range width)])
    (let* ((index (+ x (* y width))) ;; Índice unidimensional
           (color (if (= (list-ref cells index) 1) "black" "white")))
      ;; Dibuja el borde de la celda
      (send dc draw-rectangle (* x cell-size) (* y cell-size) cell-size cell-size)
      ;; Configura el pincel para rellenar la celda
      (send dc set-brush (make-object brush% color 'solid))
      ;; Rellena la celda
      (send dc draw-rectangle (* x cell-size) (* y cell-size) cell-size cell-size))))


;; Crear la ventana del juego
(define (create-game-window width height cells update-fn)
  (define paused? #t)
  (define delay 500)
  (define timer #f)

  ;; Frame principal
  (define frame (new frame% [label "Juego de la Vida"]))

  ;; Panel principal
  (define main-panel
    (new vertical-panel%
         [parent frame]))

  ;; Canvas
  (define canvas
    (new (class canvas%
           (inherit get-dc)
           (define/override (on-event event)
             (when (equal? (send event get-event-type) 'left-down)
               (let* ((mouse-x (send event get-x))
                      (mouse-y (send event get-y))
                      (cell-x (quotient mouse-x cell-size))
                      (cell-y (quotient mouse-y cell-size))
                      (index (coords->index cell-x cell-y width)))
                 (when (and (>= cell-x 0) (< cell-x width)
                            (>= cell-y 0) (< cell-y height))
                   (set! cells (list-set cells index (if (= (list-ref cells index) 1) 0 1)))
                   (send this refresh)))))
           (super-new))
         [parent main-panel]
         [stretchable-width #t]
         [stretchable-height #t]
         [paint-callback
          (lambda (canvas dc)
            (draw-grid dc cells width height))]))

  ;; Función para reiniciar el temporizador
  (define (update-timer new-interval)
    (when timer
      (send timer stop))
    (set! timer
          (new timer%
               [interval new-interval]
               [notify-callback
                (lambda ()
                  (unless paused?
                    (set! cells (update-fn cells))
                    (send canvas refresh)))])))

  ;; Iniciar el temporizador
  (update-timer delay)

  ;; Panel de control
  (define control-panel
    (new horizontal-panel%
         [parent main-panel]
         [stretchable-width #t]
         [stretchable-height #f]))

  ;; Botones del panel de control
  (new button% [parent control-panel]
       [label "Reanudar"]
       [callback
        (lambda (button event)
          (set! paused? (not paused?))
          (send button set-label (if paused? "Reanudar" "Pausa")))])

  (new button% [parent control-panel]
       [label "Siguiente"]
       [callback
        (lambda (button event)
          (when paused?
            (set! cells (update-fn cells))
            (send canvas refresh)))])

  (new button% [parent control-panel]
     [label "Limpiar pantalla"]
     [callback
      (lambda (button event)
        ;; Crear una nueva cuadrícula vacía
        (set! cells (build-list (* width height) (lambda (_) 0)))
        ;; Redibujar el canvas
        (send canvas refresh))])

  (new slider%
     [parent control-panel]
     [label "Velocidad"]
     [min-value 100]
     [max-value 1000]
     [init-value delay]
     [callback
      (lambda (slider event)
        (let ((new-delay (send slider get-value)))
          (when timer
            (send timer stop))
          (set! timer
                (new timer%
                     [interval new-delay]
                     [notify-callback
                      (lambda ()
                        (unless paused?
                          (set! cells (update-fn cells))
                          (send canvas refresh)))]))))])

  (new button% [parent control-panel]
       [label "Guardar tablero"]
       [callback
        (lambda (button event)
          (let ([filename (put-file "Selecciona un archivo para guardar")])
            (when filename
              (export-board filename cells width height))))])

  ;; Botón para volver al menú principal
  (new button% [parent control-panel]
       [label "Volver al Menú Principal"]
       [callback (lambda (button event)
                   (begin
                     (create-main-menu default-width default-height) ; Crear el menú principal
                     (send frame show #f)))]) ; Cerrar la ventana actual



  ;; Ajustar el tamaño inicial de la ventana
  (send frame resize
        (* width cell-size)
        (+ (* height cell-size) (send control-panel min-height)))

  ;; Mostrar la ventana
  (send frame show #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Matriz del título "Juego de la Vida" como lista unidimensional
(define title-grid
  (list
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
   0 0 0 0 1 1 1 1 0 0 0 0 1 0 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0
   0 0 0 0 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 0 1 0 0 0 1 1 1 0 1 0 1 0 0 0 0
   0 0 1 0 0 1 0 1 0 1 0 1 1 1 0 1 1 1 0 1 0 0 1 0 0 1 0 0 1 0 1 1 1 0 0 0 0
   0 0 0 1 1 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 1 1 0 0 0 0 1 1 0 0 1 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
   
   0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 1 0 0 0 1 1 1 0 0 0 0 0 1 0 0 1 0 0 0 0 1 1 1 0 0 1 1 1 0 0 0 0 0
   0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1 0 1 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 0 0
   0 0 0 0 0 1 0 0 1 1 0 1 0 0 0 0 0 0 1 0 0 1 0 0 1 1 0 0 0 1 1 0 1 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define title-cols 37) ; Número de columnas
(define title-rows 16)  ; Número de filas


;; Crear el menú principal
(define (create-main-menu width height)
  (define frame (new frame% [label "Juego de la Vida - Menú Principal"]))


  ;; Panel
  (define main-panel
    (new vertical-panel% [parent frame]))
  
  ;; Canvas para el título
  (define canvas
    (new canvas%
         [parent main-panel]
         [min-width (* title-cols cell-size)]
         [min-height (* title-rows cell-size)]
         [paint-callback
          (lambda (canvas dc)
            (draw-grid dc title-grid title-cols title-rows))]))
  
  
  ;; Botón para iniciar con un tablero vacío
  (new button% [parent main-panel]
       [label "Tablero vacío"]
       [callback
        (lambda (button event)
          (send frame show #f)
          (create-game-window width height (build-list (* width height) (lambda (_) 0))
                              (lambda (cells) (update-grid cells width height life-rule))))])

  ;; Botón para iniciar con un tablero aleatorio
  (new button% [parent main-panel]
       [label "Tablero aleatorio"]
       [callback
        (lambda (button event)
          (send frame show #f)
          (create-game-window width height (build-list (* width height) (lambda (_) (random 2)))
                              (lambda (cells) (update-grid cells width height life-rule))))])

  ;; Botón para cargar un tablero desde archivo en el menú principal
  (new button% [parent main-panel]
     [label "Cargar desde archivo"]
     [callback
      (lambda (button event)
        (let ([filename (get-file "Selecciona un archivo para cargar")])
          (when filename
            (let* ([lines (string-split (file->string filename) "\n")]
                   [new-height (length lines)]
                   [new-width (length (string-split (car lines)))]
                   [new-cells (import-board filename)])
              ;; Ocultar el menú principal y abrir la ventana del juego
              (send frame show #f)
              (create-game-window new-width new-height new-cells
                                  (lambda (cells) (update-grid cells new-width new-height life-rule)))))))])


  (new button% [parent main-panel]
     [label "Ajustar reglas"]
     [callback
      (lambda (button event)
        (send frame show #f)
        (create-rules-window (lambda ()
                               (send frame show #t))))])
  
  ;; Mostrar el menú principal
  (send frame show #t))
