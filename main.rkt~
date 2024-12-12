(require "game_logic.rkt"
         "game_gui.rkt")

;; Configuración inicial
(define width 20)
(define height 20)
(define initial-cells (build-list (* width height) (lambda (_) (random 2))))

;; Inicializar la ventana
(create-window width height initial-cells
               (lambda (cells) (update-grid cells width height life-rule)))
