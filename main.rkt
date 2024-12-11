(require racket/gui)

;; Convierte coordenadas (x, y) a un índice unidimensional
(define (coords->index x y width)
  (+ x (* y width)))

;; Convierte un índice unidimensional a coordenadas (x, y)
(define (index->coords index width)
  (list (modulo index width) (quotient index width)))

#| get-neighbours
Función para obtener el vecindario de una celda.
El vecindario incluye las celdas vecinas (diagonales, horizontales y verticales)
|#
(define (get-neighbours index width height)
  (define (valid? x y)
    (and (>= x 0) (< x width) (>= y 0) (< y height)))
  (define (neighbour-coords x y)
    (filter (lambda (coords) (apply valid? coords))
            (list (list (- x 1) (- y 1)) (list x (- y 1)) (list (+ x 1) (- y 1))
                  (list (- x 1) y)                     (list (+ x 1) y)
                  (list (- x 1) (+ y 1)) (list x (+ y 1)) (list (+ x 1) (+ y 1)))))
  (let ((coords (index->coords index width)))
    (map (lambda (coord) (coords->index (car coord) (cadr coord) width))
         (neighbour-coords (car coords) (cadr coords)))))

#| apply-rule
La función apply-rule toma una regla, la lista de celdas y un vecindario y aplica la regla a cada celda según el vecindario.
|#
(define (apply-rule rule cells neighbourhoods)
  (map (lambda (cell)
         (rule cell (map (lambda (index) (list-ref cells index)) (neighbourhoods cell))))
       (range (length cells))))

#|
Regla del Juego de la Vida.
Por defecto, las reglas son:

    Una celda viva con 2 o 3 vecinos vivos permanece viva.
    Una celda muerta con exactamente 3 vecinos vivos se convierte en viva.
|#
(define (life-rule cell neighbours)
  (let ((alive-neighbours (count (lambda (n) (= n 1)) neighbours)))
    (cond
      ((and (= cell 1) (or (= alive-neighbours 2) (= alive-neighbours 3))) 1)
      ((and (= cell 0) (= alive-neighbours 3)) 1)
      (else 0))))

(define (map-indexed proc lst)
  (define (loop lst index)
    (if (null? lst)
        '()
        (cons (proc index (car lst))
              (loop (cdr lst) (+ index 1)))))
  (loop lst 0))


#|
Actualizar el estado de la cuadrícula. Toma el estado actual de la cuadrícula y calcula el siguiente.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cell-size 10)

;; Dibuja la cuadrícula
(define (draw-grid dc cells width height)
  ;; Borrar el área de dibujo
  (send dc clear)
  ;; Dibujar cada celda
  (for* ([y (in-range height)] [x (in-range width)])
    (let* ((index (+ x (* y width))) ;; Índice unidimensional
           (color (if (= (list-ref cells index) 1) "black" "white")))
      ;; Configurar el pincel para rellenar
      (send dc set-brush (make-object brush% color 'solid))
      ;; Dibujar un rectángulo relleno
      (send dc draw-rectangle (* x cell-size) (* y cell-size) cell-size cell-size))))


(define (create-window width height cells update-fn)
  (define paused? #t) ;; Comenzar en pausa
  (define delay 500)  ;; Retraso inicial en milisegundos

  ;; Frame principal
  (define frame (new frame% [label "Juego de la Vida"]))

  (define canvas
  (new (class canvas%
         (inherit get-dc) ;; Para obtener el contexto de dibujo
         ;; Sobrescribir on-event para manejar eventos del mouse
         (define/override (on-event event)
           (when (equal? (send event get-event-type) 'left-down) ;; Detectar clic izquierdo
             (let* ((mouse-x (send event get-x))
                    (mouse-y (send event get-y))
                    (cell-x (quotient mouse-x cell-size))
                    (cell-y (quotient mouse-y cell-size))
                    (index (coords->index cell-x cell-y width)))
               (when (and (>= cell-x 0) (< cell-x width)
                          (>= cell-y 0) (< cell-y height))
                 ;; Alternar el estado de la celda
                 (set! cells (list-set cells index (if (= (list-ref cells index) 1) 0 1)))
                 (send this refresh))))) ;; Redibujar el canvas
         ;; Constructor
         (super-new))
       [parent frame]
       [min-width (* width cell-size)]
       [min-height (* height cell-size)]
       [paint-callback
        (lambda (canvas dc)
          ;; Dibujar el estado actual de las celdas
          (draw-grid dc cells width height))]))


  ;; Variable para el temporizador actual
  (define timer #f)

  ;; Función para actualizar el temporizador con un nuevo intervalo
  (define (update-timer new-interval)
    (when timer
      (send timer stop)) ;; Detener el temporizador anterior si existe
    (set! timer
          (new timer%
               [notify-callback
                (lambda ()
                  (unless paused?
                    (set! cells (update-fn cells)) ;; Actualizar el estado
                    (send canvas refresh)))]
               [interval new-interval])))

  ;; Inicia el temporizador con el intervalo inicial
  (update-timer delay)

  ;; Panel de control
  (define control-panel
    (new horizontal-panel%
         [parent frame]
         [alignment '(center center)])) ;; Centrar los controles

  ;; Botón de pausa/reanudar
  (define pause-button
    (new button%
         [parent control-panel]
         [label "Reanudar"] ;; Mostrar "Reanudar" al iniciar
         [callback
          (lambda (button event)
            (set! paused? (not paused?))
            (send button set-label (if paused? "Reanudar" "Pausa")))]))

  ;; Botón de siguiente estado
  (define next-button
    (new button%
         [parent control-panel]
         [label "Siguiente"]
         [callback
          (lambda (button event)
            ;; Calcular y mostrar el siguiente estado manualmente
            (when paused?
              (set! cells (update-fn cells)) ;; Actualizar el estado
              (send canvas refresh)))]))     ;; Redibujar el canvas

  ;; Botón para cargar un estado aleatorio
  (define randomize-button
    (new button%
         [parent control-panel]
         [label "Aleatorio"]
         [callback
          (lambda (button event)
            (set! cells (build-list (* width height) (lambda (_) (random 2))))
            (send canvas refresh))]))

  ;; Botón para limpiar la pantalla
  (define clear-button
    (new button%
         [parent control-panel]
         [label "Limpiar"]
         [callback
          (lambda (button event)
            (set! cells (build-list (* width height) (lambda (_) 0)))
            (send canvas refresh))]))

  ;; Slider para controlar la velocidad
  (define speed-slider
    (new slider%
         [parent control-panel]
         [label "Velocidad"]
         [min-value 50] ;; Velocidad máxima (100 ms entre actualizaciones)
         [max-value 1000] ;; Velocidad mínima (1000 ms entre actualizaciones)
         [init-value delay]
         [callback
          (lambda (slider event)
            (let ((new-delay (send slider get-value)))
              (update-timer new-delay)))]))

  ;; Ajustar el tamaño del marco
  (send frame resize
        (* width cell-size)
        (+ (* height cell-size) (send control-panel min-height)))

  ;; Mostrar la ventana
  (send frame show #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuración inicial
(define width 100)
(define height 50)
(define initial-cells (build-list (* width height) (lambda (_) (random 2))))

;; Iniciar el programa
(create-window width height initial-cells
               (lambda (cells) (update-grid cells width height life-rule)))
