#lang racket

;+---------------------------------------------+            
;|                 TDA PIXHEX                 |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a un pixhex, donde se guarda su ubicacion en los ejes X, Y, el valor hexadecimal que
; representa el color y la profunidad
; esto es guardado en una lista siguiendo el mismo orden especificado anteriormente.
; (number X number x string x number)

;+------------- CONSTRUCTORES ---------------+

; Dom: x (int), y (int), hex (string), d (number)
; Rec: Un pixrgb-d que contiene los mismos datos ingresados.
; Descripcion: Crea un pixbit en la ubicacion especificada (x, y) con el valor (hex) y profundidad (d) indicados.
; Recursion: No se usa
(define (pixhex-d x y hex d)
  (if (or (string? hex) (number? x) (number? y) (number? d))
      (list x y hex d)
      (error "Los valores ingresados no son del tipo correcto")
      )
  )

(provide (all-defined-out))