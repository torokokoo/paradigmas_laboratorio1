#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")

;+--------------------------------------------+            
;|                 TDA IMAGE                  |            
;+--------------------------------------------+

; Se crea el constructor de image

; Dom: width (int), height (int), pixmap (lista de pixbit-d|pixrgb-d|pixhex-d)
; Rec: Una lista que contiene la altura, el ancho y los pixeles respectivamente
; Descripcion: 
; Aqui solo se revisa el primer item, se confia en que se ingresaron todos los pixeles del mismo tipo.
(define image
  (lambda (width height . pixels)
    pixels
    (if (bit? (car(cdr(cdr(car pixels)))))
        (list width height "pixbit" pixels)
        (list width height "falso" pixels)
    )
  )
  )

(define (bitmap? image)
  (if (eq? (car(cdr(cdr image))) "pixbit")
      #t
      #f
      )
  )