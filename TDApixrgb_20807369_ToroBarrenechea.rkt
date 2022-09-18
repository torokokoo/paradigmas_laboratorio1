#lang racket

(define (rgb? x)
  (if (and (> x -1) (< x 256))
      #t
      #f
      )
  )

(define (pixrgb-d x y r g b d)
  (if (and (rgb? r) (rgb? g) (rgb? b))
      (list x y r g b d)
      (error "Los valores ingresados deben ser entre 0 y 255")
      )
  )

(provide (all-defined-out))