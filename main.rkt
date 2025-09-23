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
                    r c val (if has-mine? "!!!mina!!!" "libre :)"))
            (send btn set-label (if has-mine? MINE-BMP CLEAR-BMP)) ; logica: soy una mina ? soy roja. no soy una mina ? soy azul
            (send btn enable #f) ; descativa el boton al ser presionado
            )])))

; acomoda el frame
(send frame resize (+ 2 (* COLS CELL)) (+ 2 (* ROWS CELL)))
(send frame show #t) ; run !!