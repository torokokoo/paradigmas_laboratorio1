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
    (list width height pixels)
  )
  )

(define (recursion list status)
  (if (not (null? list))
    (if (and (bit? (list-ref (car list) 2)) (= (length (car list)) 4))
      (recursion (cdr list) #t)
      #f
    )
    status
  )
)

(define (bitmap? image)
      (if (= (length image) 3)
        (if (not (null? (list-ref image 2)))
          (recursion (list-ref image 2) #t)
          #f
        )
      #f
      )
  )


; Exportar las funciones del TDA
(provide (all-defined-out))