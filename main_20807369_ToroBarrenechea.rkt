#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")
(require "TDAimage_20807369_ToroBarrenechea.rkt")
(require "TDAhistogram_20807369_ToroBarrenechea.rkt")


; >-------------------------- FUNCION pixmap? bitmap? hexmap? --------------------------<
; Se encuentran en sus TDAs respectivos, debido a que por un tema de importacion se imposibilita
; tenerlos en este archivo main ya que se romperian otras funciones propias del TDA image.


; >-------------------------- FUNCION flipH --------------------------<
;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipH img)
  (list (getWidth img) (getHeight img) (recursion-fliph (third img) (getWidth img)))
)

; >-------------------------- FUNCION flipV --------------------------<
;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje Y e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipV img)
  (list (getWidth img) (getHeight img) (recursion-flipv (third img) (getHeight img)))
)

; >-------------------------- FUNCION crop --------------------------<
;Dom: img (image), x1 (number), x2 (number), y1 (number), y2 (number)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para revisar si esta dentro de las coordenadas indicadas en la funcion.
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (crop img x1 x2 y1 y2)
  (if (and (image? img) (number? x1) (number? x2) (number? y1) (number? y2))
    (list (abs (- x2 x1)) (abs (- y2 y1) ) (recursion-crop (third img) x1 x2 y1 y2))
    (error "Los valores ingresados no son del tipo correcto")
  )
)

; >-------------------------- FUNCION imgRGB->imgHex --------------------------<
;Dom: img (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para cambiar los pixrgb-d por pixhex-d
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (imgRGB->imgHex img)
  (if (and (image? img) (pixmap? img))
    (list (getWidth img) (getHeight img) (recursion-rgb->hex (third img)))
    (error "La imagen ingresada no es del tipo correcto")
  )
)

; >-------------------------- FUNCION rotate90 --------------------------<
;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e Y realizando una rotacion en 90 grados.
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (rotate90 img)
  (list (getWidth img) (getHeight img) (recursion-rotate90 (third img) (getWidth img)))
)

; >-------------------------- FUNCION compress --------------------------<
;Dom: img (image)
;Rec: image
;Desc: Hace el primer llamado recursivo para eliminar los elementos que tengan el color que mas se repite basado en el histograma
;Recursion: No se usa
(define (compress img)
  (if (pixmap? img)
    (list (getWidth img) (getHeight img) (recursion-compress-rgb (getPixels img) (caar (histogram img))))
    (list (getWidth img) (getHeight img) (recursion-compress (getPixels img) (caar (histogram img))))
  )
)

; >-------------------------- FUNCION edit --------------------------<
;Dom: img (image)
;Rec: image
;Desc: Hace el llamado para aplicar fn sobre los pixeles de la imagen
;Recursion: No se usa
(define (edit fn img)
  (if (image? img)
    (list (getWidth img) (getHeight img) (fn (getWidth img) (getHeight img) (getPixels img)))
    (error "La imagen ingresada no es del tipo correcto")
  )
)

; >-------------------------- FUNCION invertColorBit --------------------------<
;Dom: w (number?) h (number?) p [pixbit-d]
;Rec: image
;Desc: Invierte los valores de los bits de los pixeles (1->0 y 0->1)
;Recursion: No se usa
(define (invertColorBit w h p)
  (if (bitmap? (list w h p))
    (map (lambda (x) (list (first x) (second x) (abs (- (third x) 1)) (fourth x))) p)
    (error "La imagen ingresada no es del tipo bitmap?")
  )
)

; >-------------------------- FUNCION invertColorRGB --------------------------<
;Dom: w (number?) h (number?) p [pixrgb-d]
;Rec: image
;Desc: Invierte los valores de los rgb de los pixeles (255->0 y 100->155, 0->255)
;Recursion: No se usa
(define (invertColorRGB w h p)
  (if (pixmap? (list w h p))
    (map (lambda (x) (list (first x) (second x) (abs (- (third x) 255)) (abs (- (fourth x) 255)) (abs (- (fifth x) 255)) (sixth x))) p)
    (error "La imagen ingresada no es del tipo pixmap?")
  )
)

; >-------------------------- FUNCION adjustChannel --------------------------<
;Dom: get (procedure) set (procedure) fn (procedure) img (pixmap?)
;Rec: list (lista de pixeles)
;Desc: Ajusta el canal especificado en set (setR, setG, setB) segun los valores entregados por get (getR, getG, getB) aplicados sobre
;      la funcion (fn).
;Recursion: No se usa
(define (adjustChannel get set fn img)
  (if (pixmap? img)
    (map (lambda (x) (set x (fn (get x)))) (getPixels img))
    (error "La imagen ingresada no es del tipo pixmap?")
  )
)

; >-------------------------- FUNCION incCh --------------------------<
;Dom: x (rgb?)
;Rec: number (rgb?)
;Desc: Aumenta en 1 el valor de ese canal.
;Recursion: No se usa
(define (incCh x) (+ x 1))

; >-------------------------- FUNCION image->string --------------------------<
;Dom: img (image), fn (pixbit->string, pixhex->string, pixrgb->string)
;Rec: string
;Desc: Aplica la funcion fn sobre la imagen. La funcion fn devuelve un string 
;Recursion: No se usa
(define (image->string img fn)
  (fn (getWidth img) (getHeight img) (getPixels img))
)

; >-------------------------- FUNCION pixbit->string --------------------------<
;Dom: w|width (number?), h|height (number?), p|pixels (list de [pixbit-d])
;Rec: string
;Desc: Devuelve un string que representa a la imagen usando saltos de linea para las diferentes filas y tab para las diferentes columnas
;Recursion: De cola
(define (pixbit->string w h p)
  (string-append (string-join (map (lambda (e) (number->string (getBit e))) (car (sort-pixels w h p))) "\t") "\n" (recursion-bit->str (cdr (sort-pixels w h p))))
)

; >-------------------------- FUNCION pixhex->string --------------------------<
;Dom: w|width (number?), h|height (number?), p|pixels (list de [pixhex-d])
;Rec: string
;Desc: Devuelve un string que representa a la imagen usando saltos de linea para las diferentes filas y tab para las diferentes columnas
;Recursion: De cola
(define (pixhex->string w h p)
  (string-append (string-join (map (lambda (e) (getHex e)) (car (sort-pixels w h p))) "\t") "\n" (recursion-hex->str (cdr (sort-pixels w h p))))
)

; >-------------------------- FUNCION pixrgb->string --------------------------<
;Dom: w|width (number?), h|height (number?), p|pixels (list de [pixrgb-d])
;Rec: string
;Desc: Devuelve un string que representa a la imagen usando saltos de linea para las diferentes filas y tab para las diferentes columnas
;      Para esto se convirtio la imagen pixmap? en hexmap? usando imgRGB->imgHex, asi se puede reutilizar el codigo de pixhex->string
;Recursion: De cola
(define (pixrgb->string w h p)
  (string-append (string-join (map (lambda (e) (getHex e)) (car (sort-pixels w h (getPixels (imgRGB->imgHex (list w h p)))))) "\t") "\n" (recursion-hex->str (cdr (sort-pixels w h (getPixels (imgRGB->imgHex (list w h p)))))))
)

(provide (all-defined-out))