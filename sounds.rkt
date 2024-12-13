#lang racket

(require racket/sound)

(provide add-sound-to-button button-sound)

#| 
add-sound-to-button
Objetivo: Agrega un sonido a un botón existente para que reproduzca un sonido al ser pulsado.

Parámetros:
- button: Un objeto `button%` al que se le agregará el sonido.
- sound-path: String que indica la ruta del archivo de sonido.

Return:
- No retorna un valor.

Observaciones:
- El archivo de sonido debe estar en formato `.wav`.
- Si no se proporciona una ruta válida, no se agregará sonido al botón.
|#

(define (add-sound-to-button button sound-path)
  ;; Cargar el sonido si la ruta es válida
  (define sound (if (and (file-exists? sound-path)
                         (string? sound-path))
                    (make-sound sound-path)
                    #f))

  ;; Obtener el callback original del botón
  (define original-callback (send button get-callback))

  ;; Configurar el nuevo callback con sonido
  (send button set-callback
        (lambda (button event)
          ;; Reproducir el sonido si está disponible
          (when sound
            (start-sound sound))
          ;; Ejecutar el callback original
          (when original-callback
            (original-callback button event)))))