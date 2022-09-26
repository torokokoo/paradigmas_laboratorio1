#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")
(require "TDAhistogram_20807369_ToroBarrenechea.rkt")

;+--------------------------------------------+            
;|                 TDA IMAGE                  |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a una Image, donde se guarda el ancho, el alto y una lista de los pixeles que la constituyen,
; esto es guardado en una lista siguiendo el mismo orden especificado anteriormente.
; (number X number x [pixbit-d|pixrgb-d|pixhex-d]) donde [] representa una lista.

;+------------- CONSTRUCTORES ---------------+

; Dom: width (int), height (int), pixmap (lista de pixbit-d|pixrgb-d|pixhex-d)
; Rec: Una imagen de tipo image (lista)
; Descripcion: Crea una imagen con el tamano y los pixeles especificados
; Recursion: No se usa
; TODO: Revisar que se ingresen los mismos tipos de pixeles
(define image
  (lambda (width height . pixels)
    (list width height pixels)
  )
  )

;+------------- PERTENENCIA ---------------+

; Dom: image (image)
; Rec: #t o #f
; Descripcion: Revisa si se han ingresado number? para el width y height, y si corresponde a un bitmap, hexmap o pixmap
; Recursion: No se usa
(define (image? image)
  (and (number? (car image)) (number? (cadr image)) (or (bitmap? image) (hexmap? image) (pixmap? image)))
)

;+------------- SELECTORES ---------------+

;Dom: image (image)
;Rec: El ancho (number)
;Desc: Selecciona el dato width y lo retorna
;Recursion: No se usa
(define (getWidth image)
  (car image)
)

;Dom: image (image)
;Rec: El alto (number)
;Desc: Selecciona el dato height y lo retorna
;Recursion: No se usa
(define (getHeight image)
  (cadr image)
)

;Dom: image (image)
;Rec: Los pixeles (lista)
;Desc: Selecciona la lista pixeles y lo retorna
;Recursion: No se usa
(define (getPixels image)
  (caddr image)
)
  
;+------------- MODIFICADORES ---------------+
; SE ENCUENTRAN EN MAIN PARA QUE SEA MAS FACIL LA REVISION DE LAS FUNCIONES REQUERIDAS.


;+------------- OTRAS FUNCIONES ---------------+
; Funciones de apoyo a las funciones requeridas.

;Dom: w|width (number?), h|height (number?), p|pixels (list)
;Rec: list, lista de listas, cada lista interior representa un valor en el eje Y.
;Desc: A traves de la funcion sort, se ordenan respecto al eje Y, se separan en listas por cada valor del eje Y y luego se ordenan por el eje X.
;Recursion: De cola
(define (sort-pixels width height pixels)
  (map (lambda (lp) (sort lp #:key car <))
    (append (list (list-tail (sort pixels #:key cadr <) (- (length pixels) width))) (recursion-sort-pixels (list-tail (sort pixels #:key cadr >) width) width))
  )
)

;Dom: p|pixels (list), w|width (number?)
;Rec: list, lista de listas, cada lista interior representa un valor en el eje Y.
;Desc: A traves de la funcion sort, se ordenan respecto al eje Y, se separan en listas por cada valor del eje Y y luego se ordenan por el eje X.
;Recursion: De cola
(define (recursion-sort-pixels pixels width)
  (if (< (length pixels) width)
    (append (list (list-tail (sort pixels #:key cadr <) (- (length pixels) width))) (recursion-sort-pixels (list-tail (sort pixels #:key cadr >) width) width))
    (list (list-tail (sort pixels #:key cadr <) (- (length pixels) width)))
  )
)

;Dom: p|pixels (list de [pixbit-d])
;Rec: string
;Desc: Llamado recursivo para extraer los bits a traves de map y convertirlos a string, cada llamada representa una fila por ende se agrega \n
;      antes de llamar nuevamente a la funcion
;Recursion: de cola 
(define (recursion-bit->str p)
  (if (not (null? (cdr p)))
    (string-append (string-join (map (lambda (e) (number->string (getBit e))) (car p)) "\t") "\n" (recursion-bit->str (cdr p)))
    (string-append (string-join (map (lambda (e) (number->string (getBit e))) (car p)) "\t") "\n")
  )
)

;Dom: p|pixels (list de [pixhex-d])
;Rec: string
;Desc: Llamado recursivo para extraer los valores hexadecimales a traves de map y convertirlos a string, cada llamada representa 
;      una fila por ende se agrega \n antes de llamar nuevamente a la funcion
;Recursion: de cola 
(define (recursion-hex->str p)
  (if (not (null? (cdr p)))
    (string-append (string-join (map (lambda (e) (getHex e)) (car p)) "\t") "\n" (recursion-hex->str (cdr p)))
    (string-append (string-join (map (lambda (e) (getHex e)) (car p)) "\t") "\n")
  )
)


;Dom: pixels (list), width (number)
;Rec: pixels (list)
;Desc: Usando la funcion list-set invierte el valor de la ubicacion del eje X, para que se cree el efecto espejo horizontalmente
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-fliph pixels width)
  (if (not (null? (cdr pixels)))
    (append (list (list-set (car pixels) 0 (+ (- width (caar pixels)) 1))) (recursion-fliph (cdr pixels) width))
    (list (list-set (car pixels) 0 (+ (- width (caar pixels)) 1)))
  )
)

;Dom: pixels (list), height (number)
;Rec: pixels (list)
;Desc: Usando la funcion list-set invierte el valor de la ubicacion del eje Y, para que se cree el efecto espejo verticalmente
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-flipv pixels height)
  (if (not (null? (cdr pixels)))
    (append (list (list-set (car pixels) 1 (+ (- height (cadar pixels)) 1))) (recursion-flipv (cdr pixels) height))
    (list (list-set (car pixels) 1 (+ (- height (cadar pixels)) 1)))
  )
)

;Dom: pixels (list), x1 (number), x2 (number), y1 (number), y2 (number)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funciones (<=) y (>=) revisa si el pixel se encuentra dentro del
;      cuadrado formado por los puntos (x1, y1) (x2, y2), si ese es el caso los agrega a la lista de pixeles,
;      y si no los salta y sigue recursivamente con el siguiente.
;Recursion: Natural
(define (recursion-crop pixels x1 x2 y1 y2)
  (if (not (null? (cdr pixels)))
    (if (and 
          (>= (max x1 x2) (caar pixels))
          (<= (min x1 x2) (caar pixels))
          (>= (max y1 y2) (cadar pixels))
          (<= (min y1 y2) (cadar pixels))
        )
        (append (list (car pixels)) (recursion-crop (cdr pixels) x1 x2 y1 y2))
        (append (list) (recursion-crop (cdr pixels) x1 x2 y1 y2))
    )
    (if (and 
          (>= (max x1 x2) (caar pixels))
          (<= (min x1 x2) (caar pixels))
          (>= (max y1 y2) (cadar pixels))
          (<= (min y1 y2) (cadar pixels))
        )
        (list (car pixels))
        (list)
    )
  )
)

;Dom: r (number), g (number), b (number)
;Rec: hex (string)
;Desc: Pasa a traves de todos los elementos RGB y le calcula el cuociente y resto al dividirlo por 16, para asi asignarle
;      un valor exadecimal ingresando a ese indice en la lista (que se encuentra ordenada)
;Recursion: No usa
(define (RGB->hex r g b)
  (string-join 
    (list
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient r 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder r 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient g 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder g 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient b 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder b 16))
    )
  ""
  )
)

;Dom: pixels (list)
;Rec: pixels (list)
;Desc: Usando la funcion RGB->hex convierte los valores RGB en Hexadecimales y construye un TDA pixhex-d
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-rgb->hex pixels)
  (if (not (null? (cdr pixels)))
    (append (list (pixhex-d (first (car pixels)) (second (car pixels)) (RGB->hex (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) (sixth (car pixels)))) (recursion-rgb->hex (cdr pixels)))
    (list (pixhex-d (first (car pixels)) (second (car pixels)) (RGB->hex (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) (sixth (car pixels))))
  )
)

;Dom: pixels (list)
;Rec: pixels (list)
;Desc: Realiza una rotacion en 90 grados respecto al origen imaginando un plano cartesiano, o sea (x, y) -> (y, -x)
;      y le aplica una traslacion (0, width) para que el punto inferior izquierdo sea (1,1) y no un negativo
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-rotate90 pixels width)
  (if (not (null? (cdr pixels)))
    (append (list (append (list (second (car pixels)) (+ (* (first (car pixels)) -1) width 1)) (cddar pixels))) (recursion-rotate90 (cdr pixels) width))
    (list (append (list (second (car pixels)) (+ (* (first (car pixels)) -1) width 1)) (cddar pixels)))
  )
)

;Dom: pixels (list), color (bit? | hex?)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funcion eq? revisa si el pixel es del mismo color que el indicado por el histograma
;      si ese es el caso lo omite de la lista de pixeles,
;      y si no los salta (o sea que se mantiene agregado) y sigue recursivamente con el siguiente.
;Recursion: Natural
(define (recursion-compress pixels color)
  (if (not (null? (cdr pixels)))
    (if (not (eq? (third (car pixels)) color))
        (append (list (car pixels)) (recursion-compress (cdr pixels) color))
        (append (list) (recursion-compress (cdr pixels) color))
    )
    (if (not (eq? (third (car pixels)) color))
        (list (car pixels))
        (list)
    )
  )
)

;Dom: pixels (list), color (rgb?)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funcion eq-rgb? revisa si el pixel es del mismo color que el indicado por el histograma
;      si ese es el caso lo omite de la lista de pixeles,
;      y si no los salta (o sea que se mantiene agregado) y sigue recursivamente con el siguiente.
;Recursion: Natural
;NOTA: Es el mismo algoritmo de arriba, solamente que este funciona para pixmaps (ya que la comparacion de rgb? se trata de distinta forma)
(define (recursion-compress-rgb pixels color)
  (if (not (null? (cdr pixels)))
    (if (not (eq-rgb? (list (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) color))
        (append (list (car pixels)) (recursion-compress-rgb (cdr pixels) color))
        (append (list) (recursion-compress-rgb (cdr pixels) color))
    )
    (if (not (eq-rgb? (list (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) color))
        (list (car pixels))
        (list)
    )
  )
)


; Exportar las funciones del TDA
(provide (all-defined-out))