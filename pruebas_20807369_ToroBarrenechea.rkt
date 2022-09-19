#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")
(require "TDAimage_20807369_ToroBarrenechea.rkt")
(require "TDAhistogram_20807369_ToroBarrenechea.rkt")

; Datos para probar
(define bit-image (image 2 2 (pixbit-d 1 1 0 0) (pixbit-d 1 2 1 0) (pixbit-d 2 1 0 1) (pixbit-d 2 2 1 1)))
(define rgb-image (image 2 2 (pixrgb-d 1 1 0 0 0 0) (pixrgb-d 1 2 50 50 50 0) (pixrgb-d 2 1 100 100 100 1) (pixrgb-d 2 2 200 200 200 1)))
(define hex-image (image 2 2 (pixhex-d 1 1 "FFFFFF" 0) (pixhex-d 1 2 "000000" 0) (pixhex-d 2 1 "FCBA03" 1) (pixhex-d 2 2 "FC03A5" 1)))

(define bit-image-xl
  (image 4 4
    (pixbit-d 1 1 1 0)
    (pixbit-d 1 2 0 0)
    (pixbit-d 1 3 1 0)
    (pixbit-d 1 4 0 0)
    (pixbit-d 2 1 1 0)
    (pixbit-d 2 2 0 0)
    (pixbit-d 2 3 1 0)
    (pixbit-d 2 4 0 0)
    (pixbit-d 3 1 1 0)
    (pixbit-d 3 2 0 0)
    (pixbit-d 3 3 1 0)
    (pixbit-d 3 4 0 0)
    (pixbit-d 4 1 1 0)
    (pixbit-d 4 2 0 0)
    (pixbit-d 4 3 1 0)
    (pixbit-d 4 4 0 0)
  )
)