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
; Rec: Un pixhed-d que contiene los mismos datos ingresados.
; Descripcion: Crea un pixbit en la ubicacion especificada (x, y) con el valor (hex) y profundidad (d) indicados.
; Recursion: No se usa
(define (pixhex-d x y hex d)
  (if (and (hex? hex) (number? x) (number? y) (number? d))
      (list x y hex d)
      (error "Los valores ingresados no son del tipo correcto")
      )
  )


;+------------- PERTENENCIA ---------------+

; Dom: hex (string)
; Rec: #t o #f
; Descripcion: Revisa si el valor ingresado corresponde a un RGB
; Recursion: No se usa
; FALTA TRABAJARLA
(define (hex? hex)
  (and
        (= (string-length hex) 6)
        (not (not (member (substring hex 0 1) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
        (not (not (member (substring hex 1 2) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
        (not (not (member (substring hex 2 3) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
        (not (not (member (substring hex 3 4) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
        (not (not (member (substring hex 4 5) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
        (not (not (member (substring hex 5 6) (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))))
      )
)

; Dom: image (image)
; Rec: #t o #f dependiendo si la imagen esta constituida de pixhex-d
; Descripcion: Revisa si los valores que constituyen al TDA Image ingresado
; son del tipo pixhex extrayendo el dato que nos importa (la lista de pixeles) y llamando a la recursion.
; Recursion: No aplica
(define (hexmap? image)
      (if (= (length image) 3)
        (if (not (null? (list-ref image 2)))
          (recursion-hex (list-ref image 2) #t)
          #f
        )
      #f
      )
  )

;+------------- OTRAS FUNCIONES ---------------+
;Dom: list (list), status (boolean)
;Rec: #t o #f dependiendo si cumple las condiciones indicadas
;Desc: Apoyo a la funcion pixhex?, la funcion pixhex? seria el envolvente de este mismo que recibe el TDA image
;pixhex? accede a los pixeles de la imagen y llama por primera vez a la recursion. Se tienen que cumplir que
;sean del tipo hex? y que el pixhex tenga solamente 4 elementos.
;Recursion: De cola
(define (recursion-hex list status)
  (if (not (null? list))
    (if (and (= (length (car list)) 4) (hex? (list-ref (car list) 2)))
      (recursion-hex (cdr list) #t)
      #f
    )
    status
  )
)

; Exportar las funciones del TDA
(provide (all-defined-out))