#lang racket

; Se crea el constructor de image

; Dom: width (int), height (int), pixmap (lista de pixbit-d|pixrgb-d|pixhex-d)
; Rec: Una lista que contiene la altura, el ancho y los pixeles respectivamente
; Descripcion: 
(define (image width height pixmap)
  (list width height pixmap)
  )