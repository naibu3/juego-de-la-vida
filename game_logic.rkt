#lang racket


(provide update-grid get-neighbours life-rule map-indexed coords->index index->coords
         import-board export-board)

;; Convierte coordenadas (x, y) a un índice unidimensional
(define (coords->index x y width)
  (+ x (* y width)))

;; Convierte un índice unidimensional a coordenadas (x, y)
(define (index->coords index width)
  (list (modulo index width) (quotient index width)))

;; Obtener los vecinos de una celda
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

;; Regla del Juego de la Vida
(define (life-rule cell neighbours)
  (let ((alive-neighbours (count (lambda (n) (= n 1)) neighbours)))
    (cond
      ((and (= cell 1) (or (= alive-neighbours 2) (= alive-neighbours 3))) 1)
      ((and (= cell 0) (= alive-neighbours 3)) 1)
      (else 0))))

;; map-indexed: aplica una función a cada elemento con su índice
(define (map-indexed proc lst)
  (define (loop lst index)
    (if (null? lst)
        '()
        (cons (proc index (car lst))
              (loop (cdr lst) (+ index 1)))))
  (loop lst 0))

;; Función para actualizar la cuadrícula
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

Función para importar un tablero desde un archivo

Formato del archivo:

0 1 0 0
1 0 1 0
0 0 0 1
1 1 1 0

|#
(define (import-board filename)
  (with-input-from-file filename
    (lambda ()
      (let ([lines (port->lines (current-input-port))])
        (apply append
               (map (lambda (line)
                      (map string->number (string-split line)))
                    lines))))))

;; Función para exportar un tablero a un archivo
(define (export-board filename board width height)
  (with-output-to-file filename
    (lambda ()
      ;; Escribe el tablero en formato de líneas
      (for ([y (in-range height)])
        (let ([line (for/list ([x (in-range width)])
                      (number->string (list-ref board (+ x (* y width)))) )])
          (displayln (string-join line " ")))))))