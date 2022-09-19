#lang racket
;+---------------------------------------------+            
;|                 TDA PIXRGB                 |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a un pixrgb, donde se guarda su ubicacion en los ejes X, Y, sus valores R (red, rojo),
; G (green, verde), B (blue, azul) y profundidad guardado en una lista con el mismo orden especificado anteriormente.
; (number X number x number x number x number x number)

;+------------- CONSTRUCTORES ---------------+

; Dom: x (int), y (int), r (number), g (number), b (number), d (number)
; Rec: Un pixrgb-d que contiene los mismos datos ingresados.
; Descripcion: Crea un pixbit en la ubicacion especificada (x, y) con sus atributos RGB (r, g, b) y profundidad (d) indicados.
; Recursion: No se usa
(define (pixrgb-d x y r g b d)
  (if (and (rgb? r) (rgb? g) (rgb? b) (number? x) (number? y) (number? d))
      (list x y r g b d)
      (error "Los valores ingresados no son del tipo correcto")
      )
  )

;+------------- SELECTORES ---------------+

; Dom: pixrgb (pixrgb-d)
; Rec: rgb
; Desc: Extrae el valor rojo de un pixrgb-d
; Recursion: No se usa
(define (getR pixrgb) (third pixrgb))

; Dom: pixrgb (pixrgb-d)
; Rec: rgb
; Desc: Extrae el valor verde de un pixrgb-d
; Recursion: No se usa
(define (getG pixrgb) (fourth pixrgb))

; Dom: pixrgb (pixrgb-d)
; Rec: rgb
; Desc: Extrae el valor azul de un pixrgb-d
; Recursion: No se usa
(define (getB pixrgb) (fifth pixrgb))
;+------------- PERTENENCIA ---------------+

; Dom: b (int)
; Rec: #t o #f
; Descripcion: Revisa si el valor ingresado esta dentro de [0, 255]
; Recursion: No se usa
(define (rgb? x)
  (if (and (> x -1) (< x 256))
      #t
      #f
      )
  )

; Dom: image (image)
; Rec: #t o #f dependiendo si la imagen esta constituida de pixrgb-d
; Descripcion: Revisa si los valores que constituyen al TDA Image ingresado
; son del tipo pixrgb extrayendo el dato que nos importa (la lista de pixeles) y llamando a la recursion.
; Recursion: No aplica
(define (pixmap? image)
      (if (= (length image) 3)
        (if (not (null? (list-ref image 2)))
          (recursion-rgb (list-ref image 2) #t)
          #f
        )
      #f
      )
  )

;+------------- OTRAS FUNCIONES ---------------+
;Dom: list (list), status (boolean)
;Rec: #t o #f dependiendo si cumple las condiciones indicadas
;Desc: Apoyo a la funcion pixmap?, la funcion pixmap? seria el envolvente de este mismo que recibe el TDA image
;pixmap? accede a los pixeles de la imagen y llama por primera vez a la recursion. Se tienen que cumplir que
;sean del tipo rgb (entre 0 y 255), y que el pixmap tenga solamente 6 elementos (para que no se rompa cuando se ingresen
;elementos con menos de 6 como puede ser el caso del pixbit que cuenta con 4 y al intentar ingresar al (list-ref ... 4) tenga error)
;Recursion: De cola
(define (recursion-rgb list status)
  (if (not (null? list))
    (if (and (= (length (car list)) 6) (rgb? (list-ref (car list) 2)) (rgb? (list-ref (car list) 3)) (rgb? (list-ref (car list) 4)))
      (recursion-rgb (cdr list) #t)
      #f
    )
    status
  )
)



(provide (all-defined-out))