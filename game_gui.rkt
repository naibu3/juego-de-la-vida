#lang racket

(require racket/gui
         "game_logic.rkt")

(provide create-main-menu create-game-window)

(define cell-size 10)

;; Dibujar la cuadrícula
(define (draw-grid dc cells width height)
  (send dc clear)
  (for* ([y (in-range height)] [x (in-range width)])
    (let* ((index (+ x (* y width))) ;; Índice unidimensional
           (color (if (= (list-ref cells index) 1) "black" "white")))
      (send dc set-brush (make-object brush% color 'solid))
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

  ;; Ajustar el tamaño inicial de la ventana
  (send frame resize
        (* width cell-size)
        (+ (* height cell-size) (send control-panel min-height)))

  ;; Mostrar la ventana
  (send frame show #t))

;; Crear el menú principal
(define (create-main-menu width height)
  (define frame (new frame% [label "Juego de la Vida - Menú Principal"]))

  (define main-panel
    (new vertical-panel%
         [parent frame]))

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


  ;; Mostrar el menú principal
  (send frame show #t))
