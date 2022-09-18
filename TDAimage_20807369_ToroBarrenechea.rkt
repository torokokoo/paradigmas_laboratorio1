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
(define (flipH image)
)

; Exportar las funciones del TDA
(provide (all-defined-out))