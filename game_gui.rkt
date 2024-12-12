#lang racket

(require racket/gui
         "game_logic.rkt")

(provide create-window)

(define cell-size 10)

;; Dibujar la cuadrícula
(define (draw-grid dc cells width height)
  (send dc clear)
  (for* ([y (in-range height)] [x (in-range width)])
    (let* ((index (+ x (* y width))) ;; Índice unidimensional
           (color (if (= (list-ref cells index) 1) "black" "white")))
      (send dc set-brush (make-object brush% color 'solid))
      (send dc draw-rectangle (* x cell-size) (* y cell-size) cell-size cell-size))))

;; Crear la ventana principal
(define (create-window width height cells update-fn)
  (define paused? #t)
  (define delay 500) ;; Retraso inicial del temporizador
  (define timer #f)  ;; Variable para guardar el temporizador actual

  ;; Frame principal
  (define frame (new frame% [label "Juego de la Vida"]))

  ;; Panel principal: contiene el canvas y el panel de control
  (define main-panel
    (new vertical-panel%
         [parent frame]))

  ;; Canvas para la cuadrícula
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
                   ;; Alternar el estado de la celda
                   (set! cells (list-set cells index (if (= (list-ref cells index) 1) 0 1)))
                   (send this refresh)))))
           (super-new))
         [parent main-panel]
         [stretchable-width #t]  ;; Se ajusta al ancho
         [stretchable-height #t] ;; Se ajusta al alto
         [paint-callback
          (lambda (canvas dc)
            (draw-grid dc cells width height))]))

  ;; Función para reiniciar el temporizador con un nuevo intervalo
  (define (update-timer new-interval)
    (when timer
      (send timer stop)) ;; Detener el temporizador actual si existe
    (set! timer
          (new timer%
               [interval new-interval]
               [notify-callback
                (lambda ()
                  (unless paused?
                    (set! cells (update-fn cells))
                    (send canvas refresh)))])))

  ;; Iniciar el temporizador con el intervalo inicial
  (update-timer delay)

  ;; Panel de control con tamaño fijo
  (define control-panel
    (new horizontal-panel%
         [parent main-panel]
         [stretchable-width #t]  ;; Se ajusta al ancho
         [stretchable-height #f] ;; No cambia su altura
         [alignment '(center center)])) ;; Panel de control fijo

  ;; Botón de pausa/reanudar
  (new button% [parent control-panel]
       [label "Reanudar"]
       [callback
        (lambda (button event)
          (set! paused? (not paused?))
          (send button set-label (if paused? "Reanudar" "Pausa")))])

  ;; Botón de siguiente estado
  (new button% [parent control-panel]
       [label "Siguiente"]
       [callback
        (lambda (button event)
          (when paused?
            (set! cells (update-fn cells))
            (send canvas refresh)))])

  ;; Slider para controlar la velocidad
  (new slider%
       [parent control-panel]
       [label "Velocidad"]
       [min-value 100] ;; Velocidad máxima (100 ms entre actualizaciones)
       [max-value 1000] ;; Velocidad mínima (1000 ms entre actualizaciones)
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

  ;; Ajustar el tamaño inicial de la ventana
  (send frame resize
        (* width cell-size) ;; Ancho del tablero
        (+ (* height cell-size) (send control-panel min-height))) ;; Altura del tablero + control-panel

  ;; Mostrar la ventana
  (send frame show #t))
