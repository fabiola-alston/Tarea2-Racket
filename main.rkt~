
#lang racket/gui
(require racket/list
         racket/set)

; propiedades por que en racket no hay variables :)
(define ROWS 10)
(define COLS 10)
(define CELL 35) ;; tamano de cada celda

; cantidad de minas, esto define la "dificultad"
(define MINE-RATIO 0.15) ; ahorita esta puesto para 15% pero luego se mete a logica de dificultad
(define MINE-COUNT (max 1 (inexact->exact (round (* ROWS COLS MINE-RATIO))))) ; con esto se calcula la cantidad de minas

; def de matriz, con respecto a cantidad de filas y columnas
; crea sistema tipo (x,y), con (0, 0) siendo la esquina izquierda superior y (ROW-1, COLUMN-1) siendo la esquina inferior derecha. 
(define mat
  (for/list ([r (in-range ROWS)])
    (for/list ([c (in-range COLS)])
      (list r c)))) ; formatea a lista 

; funcion de acceso a matriz 
(define (mat-ref m r c)
  (list-ref (list-ref m r) c))

; funcion para generacion aleatoria de posiciones de minas con respecto a MINE-COUNT (k)
(define (random-mines rows cols k)
  (define total (* rows cols)) ; calcula total de celdas
  (define idxs (shuffle (range total))) ; agarra un rango de las celdas [0, total-1], las mezcla (shuffle) y guardas eso en idxs
  (define chosen (take idxs (min k total))) ; escoje las primeras k itemes de idxs, dandonos resultados aleatorios
  (list->set
   (for/list ([i chosen])
     (define r (quotient i cols))
     (define c (remainder i cols))
     (cons r c)))) ; las guarda en un set de minas como un par ordenado

; aqui "setea" la generacion aleatoria de minas con esa funcion de arriba usando los datos reales del juego (rows columns etc)
(define mine-set (random-mines ROWS COLS MINE-COUNT))

; esto es como una """textura""" que se le puede poner al boton, que sirve como imagen ya que racket es demasiado tieso y el ui es criminal
(define (solid-bitmap w h color)
  (define bm (make-bitmap w h))
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-brush (make-object brush% color 'solid))
  (send dc set-pen   (make-object pen% color 1 'transparent))
  (send dc draw-rectangle 0 0 w h)
  (send dc set-bitmap #f)
  bm)

(define EMPTY-BMP (solid-bitmap CELL CELL (make-color 242 242 242))) ; color inicial (todos igual)
(define CLEAR-BMP (solid-bitmap CELL CELL (make-color 138 178 227)))     ; libre al hacer clic
(define MINE-BMP   (solid-bitmap CELL CELL (make-color 189 57 47)))       ; mina al hacer clic

;; Selector 
(define (choose b x y) (or (and b x) y))



;; --- Bandera ---
(define FLAG-BMP (solid-bitmap CELL CELL (make-color 247 223 30))) ; amarillo

;; set de posiciones con bandera y modo bandera (checkbox)
(define FLAG-SET (box (set)))   ; set de (cons r c)
(define FLAG-MODE? (box #f))

;; utilidades 
(define (flagged? r c) (set-member? (unbox FLAG-SET) (cons r c)))
(define (set-toggle s k) (choose (set-member? s k) (set-remove s k) (set-add s k)))

;; alternar bandera para un botón en (r,c)
(define (toggle-flag! r c btn)
  (let* ([pos (cons r c)]
         [s   (unbox FLAG-SET)]
         [s2  (set-toggle s pos)]
         [_   (set-box! FLAG-SET s2)]
         [f?  (set-member? s2 pos)])
    (send btn set-label (choose f? FLAG-BMP EMPTY-BMP))))





;; ----- helpers para números/vecinos -----

;; Dibuja número sobre fondo azul - CORREGIDO
(define (number-bitmap n)
  (define bm (make-bitmap CELL CELL))
  (define dc (new bitmap-dc% [bitmap bm]))
  ; FONDO AZUL
  (send dc set-brush (make-object brush% (make-color 138 178 227) 'solid))
  ; PEN CORREGIDO - usa un color en lugar de 'transparent
  (send dc set-pen (make-object pen% (make-color 0 0 0) 1 'transparent))
  (send dc draw-rectangle 0 0 CELL CELL)
  ; TEXTO
  (send dc set-text-foreground "black")
  (send dc set-font (make-object font% 14 'default 'normal 'bold))
  (send dc draw-text (number->string n) 
        (- (quotient CELL 2) 5)  ; Centrado horizontal
        (- (quotient CELL 3) 5)) ; Centrado vertical
  (send dc set-bitmap #f)
  bm)

;; Vecinos (8-dir) dentro de límites
(define (neighbors r c)
  (define deltas '((-1 -1) (-1 0) (-1 1)
                   ( 0 -1)         ( 0 1)
                   ( 1 -1) ( 1 0)  ( 1 1)))
  (reverse
   (for/fold ([acc '()]) ([d deltas])
     (define nr (+ r (first d)))
     (define nc (+ c (second d)))
     (define in? (and (<= 0 nr) (< nr ROWS) (<= 0 nc) (< nc COLS)))
     (choose in? (cons (cons nr nc) acc) acc))))


;; Cuenta minas adyacentes usando tu mine-set 
(define (adjacent-mines r c)
  (for/fold ([s 0]) ([p (neighbors r c)])
    (+ s (choose (set-member? mine-set p) 1 0))))



; ya todo aqui es puro ui y funcionamiento de botones y asi
(define frame
  (new frame%
       [label "minesweeper :o"]
       [stretchable-width #f] ; no se puede ajustar tamano
       [stretchable-height #f])) ; aqui tampoco 

; este vertical panel es el panel donde se van a ubicar todos los paneles horizontales que forman las filas
(define root
  (new vertical-panel%
       [parent frame]
       [spacing 0]
       [horiz-margin 0]
       [vert-margin 0]
       [stretchable-width #f] ; tampoco se pueden ajustar
       [stretchable-height #f])) ; ^
;; Mapa coord -> botón (para flood-fill mínimo)
(define BTN-HASH (make-hash))


;; Flag para saber si el juego ya terminó (derrota o victoria)
(define GAME-ENDED? (box #f))



;; Deshabilita TODOS los botones del tablero caundo se pisa una mina 
(define (disable-board!)
  (for ([r (in-range ROWS)])
    (for ([c (in-range COLS)])
      (let ([btn (hash-ref BTN-HASH (cons r c) #f)])
        (and btn (send btn enable #f))))))




;; ¿Ya ganó? (todas las celdas NO-mina están deshabilitadas)
(define (win?)
  (for/fold ([ok #t]) ([r (in-range ROWS)])
    (for/fold ([ok2 ok]) ([c (in-range COLS)])
      (let* ([p      (cons r c)]
             [is-m   (set-member? mine-set p)]
             [btn    (hash-ref BTN-HASH p #f)]
             [opened (and btn (not (send btn is-enabled?)))])
        (and ok2 (or is-m opened))))))


;; barra superior con checkbox para activar modo bandera
(define top
  (new horizontal-panel%
       [parent root] [spacing 6] [horiz-margin 4] [vert-margin 4]
       [stretchable-width #f] [stretchable-height #f]))

(new check-box%
     [parent top]
     [label "Modo bandera"]
     [value (unbox FLAG-MODE?)]
     [callback (lambda (cb _)
                 (set-box! FLAG-MODE? (send cb get-value)))])




(for ([r (in-range ROWS)])
  ; estos son los paneles horizontales que se mencionaron encima de la funcion de root, lit son filas
  (define row
    (new horizontal-panel%
         [parent root]
         [spacing 0]
         [horiz-margin 0]
         [vert-margin 0]
         [stretchable-width #f]
         [stretchable-height #f]))

  ; ok vamos buscando columna por columna
  (for ([c (in-range COLS)])
    (define val (mat-ref mat r c)) ;; recordar que r es el row actual y c es la columna 
    (define has-mine? (set-member? mine-set (cons r c))) ; se acuerda antes la vara que usamos para definir el set de minas ? basicamente esto lo consulta
    ; basicamente esta linea de arriba define la funcion para ver si hay una mina en la posicion en la que esta el boton (valor #t o #f)


    (define btn
  (new button%
       [parent row] ; le dice al boton 'tu padre es la fila en la que ahorita estamos'
       [label EMPTY-BMP] ; le pone textura gris
       [min-width  CELL] ; hacerla cuadrada
       [min-height CELL] ; same here
       [stretchable-width #f]
       [stretchable-height #f]
       [callback ; callback --> "cuando soy presionado"
        (lambda (btn _evt)
          (printf "row:~a, column:~a = ~a ~a\n"
                  r c val (choose has-mine? "!!!mina!!!" "libre :)"))

          ;; si está en modo bandera y el botón está habilitado → alternar bandera y no abrir
          (and (unbox FLAG-MODE?)
               (send btn is-enabled?)
               (begin
                 (toggle-flag! r c btn)
                 'done))

          ;; si NO está en modo bandera, y el botón está habilitado, y la celda NO está marcada → abrir
          (and (not (unbox FLAG-MODE?))
               (send btn is-enabled?)
               (not (flagged? r c))
               (begin
                 (send btn set-label (choose has-mine? MINE-BMP CLEAR-BMP)) ; logica: soy una mina ? soy roja. no soy una mina ? soy azul
                 (send btn enable #f) ; descativa el boton al ser presionado

                 ;; Derrota  (marca fin y deshabilita tablero)
                 (and has-mine?
                      (begin
                        (set-box! GAME-ENDED? #t)
                        (disable-board!)
                        (message-box "Fin del juego" "¡Perdiste! Pisaste una mina." frame '(ok))))

                 ;; No-mine: número y flood-fill si es 0
                 (and (not has-mine?)
                      (let ([n (adjacent-mines r c)])
                        (send btn set-label (choose (= n 0) CLEAR-BMP (number-bitmap n)))
                        (and (= n 0) (flood-open! r c))))))

          ;; Victoria (solo si no terminó por derrota; al final del click)
          (and (not (unbox GAME-ENDED?))
               (win?)
               (begin
                 (set-box! GAME-ENDED? #t)
                 (message-box "¡Ganaste!" "¡Limpiaste el tablero!" frame '(ok)))))])) 

;; guarda el botón
(hash-set! BTN-HASH (cons r c) btn)

;; guarda el botón
(hash-set! BTN-HASH (cons r c) btn))
)  

;; Abre vecinos recursivamente cuando una celda es 0
(define (flood-open! r c)
  (for ([p (neighbors r c)])
    (let ([btn  (hash-ref BTN-HASH p #f)])
      (and btn
           (and (send btn is-enabled?)           ; aún no abierta
                (let* ([pr  (car p)]
                       [pc  (cdr p)]
                       [has (set-member? mine-set p)]
                       [flagged (set-member? (unbox FLAG-SET) p)]
                       [n   (adjacent-mines pr pc)])
                  (and (not has)
                       (begin
                         (send btn set-label (choose (= n 0) CLEAR-BMP (number-bitmap n)))
                         (send btn enable #f)
                         (and (= n 0) (flood-open! pr pc))))))))))





; acomoda el frame
(send frame resize (+ 2 (* COLS CELL)) (+ 2 (* ROWS CELL)))
(send frame show #t) ; run !!