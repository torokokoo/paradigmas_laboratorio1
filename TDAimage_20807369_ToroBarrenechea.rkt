#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")

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

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipH img)
  (list (getWidth img) (getHeight img) (recursion-fliph (third img) (getWidth img)))
)

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje Y e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipV img)
  (list (getWidth img) (getHeight img) (recursion-flipv (third img) (getHeight img)))
)

;Dom: img (image), x1 (number), x2 (number), y1 (number), y2 (number)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para revisar si esta dentro de las coordenadas indicadas en la funcion.
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (crop img x1 x2 y1 y2)
  (if (and (image? img) (number? x1) (number? x2) (number? y1) (number? y2))
    (list (getWidth img) (getHeight img) (recursion-crop (third img) x1 x2 y1 y2))
    (error "Los valores ingresados no son del tipo correcto")
  )
)

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

;+------------- OTRAS FUNCIONES ---------------+

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

; Exportar las funciones del TDA
(provide (all-defined-out))