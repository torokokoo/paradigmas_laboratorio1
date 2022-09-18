#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")

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
  
; Exportar las funciones del TDA
(provide (all-defined-out))