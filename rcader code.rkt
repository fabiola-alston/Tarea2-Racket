#lang racket
(require r-cade)
(require racket/match)

; hola, aqui dejo los links para poder descargar r-cader
; de ahi usted me dice cual cree es mejor usar, este la verdad se ve mas
; bonito pero habria que hacer mas configuracion como con coordenadas
; en vez de botones :/ igual dejo la opcion por si acaso

; https://r-cade.io/
; https://r-cade.io/setup --> en este vienen todos los links para descarga
; https://r-cade.io/tutorials --> y aqui una explicacion breve / tutorial

(define jump (action btn-select))
(define talking (action btn-z #t))
(define drawing (action btn-x #t))

(define boop (sweep 440   ; start frequency
                    300   ; end frequency
                    0.1   ; duration
                    (voice sawtooth-wave z-envelope)))

(define theme
  (music "E4-B3C4D-CB3A-AC4E-DCB3-C4D-E-C-A3-A-.D4-FA-GFE-CE-DCB3-BC4D-E-C-A3-A-."
         #:tempo 280))

(define (do-boop)
  (btn-start #t 3))

(define (random-color x)
  (cond
    [(>= x 17) 0]
    [(< x 17) (+ x 1)]))

(define i 0)

(define (game-loop)
  (cls)

  (text 2 2 "hola, presiona z para hablar")
  (text 2 10 "x para dibujar corazon y enter para tocar sonido")
  (text 2 20 "space para saltar")
  (text 2 30 "y enter para tocar sonido")


  (when (jump)
    (text 2 50 "jumped!"))

  (when (talking)
    (text 2 50 "blablabla..."))

  (when (drawing)
    (set! i (random-color i))
    (let ([x (mouse-x)]
         [y (mouse-y)])
    (color i)
    (draw x y '(#b01100110
                #b11111111
                #b11111111
                #b01111110
                #b00111100
                #b00011000))))
  
   (when (do-boop)
    (play-sound boop)))

  (define (start-music)
    (play-music theme))

(run game-loop 200 128)


