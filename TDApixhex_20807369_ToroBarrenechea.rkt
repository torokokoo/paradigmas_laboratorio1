#lang racket

(define (pixhex x y hex)
  (if (string? hex)
      (list x y hex)
      (error "No has ingreado un hexadecimal")
      )
  )