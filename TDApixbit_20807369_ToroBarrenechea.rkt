#lang racket

(define (bit? b)
  (if (or (= b 0) (= b 1))
      #t
      #f)
  )

(define (pixbit x y bit)
  (if (bit? bit)
      (list x y bit)
      (error "No has ingresado un bit")
      )
  )


; Exportar las funciones del TDA
(provide (all-defined-out))