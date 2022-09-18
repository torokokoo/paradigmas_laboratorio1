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
  
;+------------- OTRAS FUNCIONES ---------------+
;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipH img)
  (list (getWidth img) (getHeight img) (recursion-fliph (third img) (getWidth img)))
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

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje Y e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipV img)
  (list (getWidth img) (getHeight img) (recursion-flipv (third img) (getHeight img)))
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

; Exportar las funciones del TDA
(provide (all-defined-out))