#lang racket/gui

; tamano del grid
(define GRID 10)
; tamano de cada boton
(define CELL 35)

(define frame
  (new frame%
       [label "Minesweeper"]
       [stretchable-width #f]
       [stretchable-height #f]))

;; panel raíz (supuestamente sin espacios entre botones, esta demasiada tiesa la vara)
(define root
  (new vertical-panel%
       [parent frame]
       [spacing 0]
       [horiz-margin 0]
       [vert-margin 0]
       [stretchable-width #f]
       [stretchable-height #f]))

(for ([r (in-range GRID)])
  (define row
    (new horizontal-panel%
         [parent root]
         [spacing 0]
         [horiz-margin 0]
         [vert-margin 0]
         [stretchable-width #f]
         [stretchable-height #f]))
  (for ([c (in-range GRID)])
    (new button%
         [parent row]
         [label ""]

         ; para hacer botones cuadrados (se lo esta pasando por el culo ahorita)
         [min-width  CELL]         
         [min-height CELL]
         
         ; para que no se pueda estirar
         [stretchable-width #f]     
         [stretchable-height #f]

         [callback (λ (_btn _evt) (void))])))

;; ajuste ventana
(send frame resize (+ 2 (* GRID CELL)) (+ 2 (* GRID CELL)))

; corre programa (ventana)
(send frame show #t)