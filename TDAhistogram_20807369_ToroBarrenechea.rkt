#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")

;+--------------------------------------------+            
;|               TDA HISTOGRAM                |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a un histograma, donde se guarda la cantidad de repeteciones de los colores en la imagen,
; a partir del ingreso de un TDA image
; (image)

;+------------- CONSTRUCTORES ---------------+

; Dom: img (image)
; Rec: histogram (list)
; Descripcion: Retorna una lista con la cantidad de veces que se repiten los colores de la forma '((color . veces))
; Recursion: No se usa
(define (histogram img)
  (if (or (bitmap? img) (hexmap? img))
    (remove-duplicates 
      (map (lambda (i) 
        (list i (count (lambda (j) (eq? i j)) (map (lambda (e) (list-ref e 2)) (third img)
      )))) (map (lambda (e) (list-ref e 2)) (third img)))
    )
    (remove-duplicates 
      (map (lambda (i) 
        (list i (count (lambda (j) (eq-rgb? i j)) (map (lambda (e) (list (getR e) (getG e) (getB e))) (third img)
      )))) (map (lambda (e) (list (getR e) (getG e) (getB e))) (third img)))
    )
  )
)

;+------------- OTRAS FUNCIONES ---------------+
; Dom: lst1 (list), lst2 (list)
; Rec: #t o #f (?boolean)
; Desc: Compara dos listas que contienen valores RGB (r (rgb?), g (rgb?), b (rgb?)) y revisa que sean iguales
; Recursion: No se usa
(define (eq-rgb? lst1 lst2)
  (and (= (first lst1) (first lst2)) (= (second lst1) (second lst2)) (= (third lst1) (third lst2)))
)

(provide (all-defined-out))