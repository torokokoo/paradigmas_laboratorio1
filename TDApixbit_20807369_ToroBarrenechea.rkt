#lang racket

;+---------------------------------------------+            
;|                 TDA PIXBIT                 |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a un pixbit, donde se guarda su ubicacion en los ejes X, Y, el bit que representa si es blanco
; o negro y la profundidad
; esto es guardado en una lista siguiendo el mismo orden especificado anteriormente.
; (number X number x number x number)

;+------------- CONSTRUCTORES ---------------+

; Dom: x (int), y (int), bit (number), d (number)
; Rec: Un pixbit-d que contiene los mismos datos ingresados.
; Descripcion: Crea un pixbit en la ubicacion especificada (x, y) con el valor (bit) y profundidad (d) indicados.
; Recursion: No se usa
(define (pixbit-d x y bit d)
  (if (and (bit? bit) (number? x) (number? y) (number? d))
      (list x y bit d)
      (error "Los valores ingresados no son del tipo correcto")
      )
  )


;+------------- PERTENENCIA ---------------+

; Dom: b (int)
; Rec: #t o #f dependiendo si el valor ingresado es un bit
; Descripcion: Revisa si el valor ingresado es un bit (0 o 1)
; Recursion: No se usa
(define (bit? b)
  (if (and (number? b) (or (= b 0) (= b 1)))
      #t
      #f)
  )

; Dom: image (image)
; Rec: #t o #f dependiendo si la imagen esta constituida de pixbits
; Descripcion: Revisa si los valores que constituyen al TDA Image ingresado
; son del tipo bitmap extrayendo el dato que nos importa (la lista de pixeles) y llamando a la recursion.
; Recursion: No aplica
(define (bitmap? image)
      (if (= (length image) 3)
        (if (not (null? (list-ref image 2)))
          (recursion-bit (list-ref image 2) #t)
          #f
        )
      #f
      )
  )

;+------------- OTRAS FUNCIONES ---------------+
;Dom: list (list), status (boolean)
;Rec: #t o #f dependiendo si cumple las condiciones indicadas
;Desc: Apoyo a la funcion bitmap?, la funcion bitmap? seria el envolvente de este mismo que recibe el TDA image
;bitmap? accede a los pixeles de la imagen y llama por primera vez a la recursion. Se tienen que cumplir que
;sea de un tipo bit, y que el pixmap tenga solamente 4 elementos (para que no se confunda con un pixrgb-d)
;Recursion: De cola
(define (recursion-bit list status)
  (if (not (null? list))
    (if (and (bit? (list-ref (car list) 2)) (= (length (car list)) 4))
      (recursion-bit (cdr list) #t)
      #f
    )
    status
  )
)


; Exportar las funciones del TDA
(provide (all-defined-out))