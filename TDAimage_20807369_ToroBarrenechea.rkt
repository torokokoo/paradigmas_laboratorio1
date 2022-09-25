#lang racket

(require "TDApixbit_20807369_ToroBarrenechea.rkt")
(require "TDApixrgb_20807369_ToroBarrenechea.rkt")
(require "TDApixhex_20807369_ToroBarrenechea.rkt")

;+--------------------------------------------+            
;|                 TDA IMAGE                  |            
;+--------------------------------------------+

;+------------- REPRESENTACION---------------+
; Este TDA corresponde a una Image, donde se guarda el ancho, el alto y una lista de los pixeles que la constituyen,
; esto es guardado en una lista siguiendo el mismo orden especificado anteriormente.
; (number X number x [pixbit-d|pixrgb-d|pixhex-d]) donde [] representa una lista.

;+------------- CONSTRUCTORES ---------------+

; Dom: width (int), height (int), pixmap (lista de pixbit-d|pixrgb-d|pixhex-d)
; Rec: Una imagen de tipo image (lista)
; Descripcion: Crea una imagen con el tamano y los pixeles especificados
; Recursion: No se usa
; TODO: Revisar que se ingresen los mismos tipos de pixeles
(define image
  (lambda (width height . pixels)
    (list width height pixels)
  )
  )

;+------------- PERTENENCIA ---------------+

; Dom: image (image)
; Rec: #t o #f
; Descripcion: Revisa si se han ingresado number? para el width y height, y si corresponde a un bitmap, hexmap o pixmap
; Recursion: No se usa
(define (image? image)
  (and (number? (car image)) (number? (cadr image)) (or (bitmap? image) (hexmap? image) (pixmap? image)))
)

;+------------- SELECTORES ---------------+

;Dom: image (image)
;Rec: El ancho (number)
;Desc: Selecciona el dato width y lo retorna
;Recursion: No se usa
(define (getWidth image)
  (car image)
)

;Dom: image (image)
;Rec: El alto (number)
;Desc: Selecciona el dato height y lo retorna
;Recursion: No se usa
(define (getHeight image)
  (cadr image)
)

;Dom: image (image)
;Rec: Los pixeles (lista)
;Desc: Selecciona la lista pixeles y lo retorna
;Recursion: No se usa
(define (getPixels image)
  (caddr image)
)
  
;+------------- MODIFICADORES ---------------+

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipH img)
  (list (getWidth img) (getHeight img) (recursion-fliph (third img) (getWidth img)))
)

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje Y e invertirlos
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (flipV img)
  (list (getWidth img) (getHeight img) (recursion-flipv (third img) (getHeight img)))
)

;Dom: img (image), x1 (number), x2 (number), y1 (number), y2 (number)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para revisar si esta dentro de las coordenadas indicadas en la funcion.
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (crop img x1 x2 y1 y2)
  (if (and (image? img) (number? x1) (number? x2) (number? y1) (number? y2))
    (list (getWidth img) (getHeight img) (recursion-crop (third img) x1 x2 y1 y2))
    (error "Los valores ingresados no son del tipo correcto")
  )
)

;Dom: img (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para cambiar los pixrgb-d por pixhex-d
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-flipv) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (imgRGB->imgHex img)
  (if (and (image? img) (pixmap? img))
    (list (getWidth img) (getHeight img) (recursion-rgb->hex (third img)))
    (error "La imagen ingresada no es del tipo correcto")
  )
)

;Dom: image (image)
;Rec: image (image)
;Desc: Toma una imagen y le aplica llamados recursivos para modificar los valores del eje X e Y realizando una rotacion en 90 grados.
;Recursion: No se usa
;NOTA: No se puede usar el constructor (image) debido a que (recursion-fliph) retorna una lista, y si se usa el
;      constructor el resultado seria (width height ((pixmaps))), o sea, dos listas envuelven a pixmaps en vez de una 
(define (rotate90 img)
  (list (getWidth img) (getHeight img) (recursion-rotate90 (third img) (getWidth img)))
)

; Funcion para externalizar el sort, la dejo como recordatorio en caso de utilizarla
;(define (sort-histogram h)
;  (sort h #:key cadr >)
;)

;Dom: img (image)
;Rec: image
;Desc: Hace el primer llamado recursivo para eliminar los elementos que tengan el color que mas se repite basado en el histograma
;Recursion: No se usa
(define (compress img)
  (if (pixmap? img)
    (list (getWidth img) (getHeight img) (recursion-compress-rgb (getPixels img) (caar (histogram img))))
    (list (getWidth img) (getHeight img) (recursion-compress (getPixels img) (caar (histogram img))))
  )
)

;Dom: img (image)
;Rec: image
;Desc: Hace el llamado para aplicar fn sobre los pixeles de la imagen
;Recursion: No se usa
(define (edit fn img)
  (if (image? img)
    (list (getWidth img) (getHeight img) (fn (getWidth img) (getHeight img) (getPixels img)))
    (error "La imagen ingresada no es del tipo correcto")
  )
)

;+------------- OTRAS FUNCIONES ---------------+


;Dom: w (number?) h (number?) p [pixbit-d]
;Rec: image
;Desc: Invierte los valores de los bits de los pixeles (1->0 y 0->1)
;Recursion: No se usa
(define (invertColorBit w h p)
  (if (bitmap? (list w h p))
    (map (lambda (x) (list (first x) (second x) (abs (- (third x) 1)) (fourth x))) p)
    (error "La imagen ingresada no es del tipo bitmap?")
  )
)

;Dom: w (number?) h (number?) p [pixrgb-d]
;Rec: image
;Desc: Invierte los valores de los rgb de los pixeles (255->0 y 100->155, 0->255)
;Recursion: No se usa
(define (invertColorRGB w h p)
  (if (pixmap? (list w h p))
    (map (lambda (x) (list (first x) (second x) (abs (- (third x) 255)) (abs (- (fourth x) 255)) (abs (- (fifth x) 255)) (sixth x))) p)
    (error "La imagen ingresada no es del tipo pixmap?")
  )
)

;Dom: get (procedure) set (procedure) fn (procedure) img (pixmap?)
;Rec: list (lista de pixeles)
;Desc: Ajusta el canal especificado en set (setR, setG, setB) segun los valores entregados por get (getR, getG, getB) aplicados sobre
;      la funcion (fn).
;Recursion: No se usa
(define (adjustChannel get set fn img)
  (if (pixmap? img)
    (map (lambda (x) (set x (fn (get x)))) (getPixels img))
    (error "La imagen ingresada no es del tipo pixmap?")
  )
)

;Dom: x (rgb?)
;Rec: number (rgb?)
;Desc: Aumenta en 1 el valor de ese canal.
;Recursion: No se usa
(define (incCh x) (+ x 1))

;Dom: pixels (list), width (number)
;Rec: pixels (list)
;Desc: Usando la funcion list-set invierte el valor de la ubicacion del eje X, para que se cree el efecto espejo horizontalmente
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-fliph pixels width)
  (if (not (null? (cdr pixels)))
    (append (list (list-set (car pixels) 0 (+ (- width (caar pixels)) 1))) (recursion-fliph (cdr pixels) width))
    (list (list-set (car pixels) 0 (+ (- width (caar pixels)) 1)))
  )
)

;Dom: pixels (list), height (number)
;Rec: pixels (list)
;Desc: Usando la funcion list-set invierte el valor de la ubicacion del eje Y, para que se cree el efecto espejo verticalmente
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-flipv pixels height)
  (if (not (null? (cdr pixels)))
    (append (list (list-set (car pixels) 1 (+ (- height (cadar pixels)) 1))) (recursion-flipv (cdr pixels) height))
    (list (list-set (car pixels) 1 (+ (- height (cadar pixels)) 1)))
  )
)

;Dom: pixels (list), x1 (number), x2 (number), y1 (number), y2 (number)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funciones (<=) y (>=) revisa si el pixel se encuentra dentro del
;      cuadrado formado por los puntos (x1, y1) (x2, y2), si ese es el caso los agrega a la lista de pixeles,
;      y si no los salta y sigue recursivamente con el siguiente.
;Recursion: Natural
(define (recursion-crop pixels x1 x2 y1 y2)
  (if (not (null? (cdr pixels)))
    (if (and 
          (>= (max x1 x2) (caar pixels))
          (<= (min x1 x2) (caar pixels))
          (>= (max y1 y2) (cadar pixels))
          (<= (min y1 y2) (cadar pixels))
        )
        (append (list (car pixels)) (recursion-crop (cdr pixels) x1 x2 y1 y2))
        (append (list) (recursion-crop (cdr pixels) x1 x2 y1 y2))
    )
    (if (and 
          (>= (max x1 x2) (caar pixels))
          (<= (min x1 x2) (caar pixels))
          (>= (max y1 y2) (cadar pixels))
          (<= (min y1 y2) (cadar pixels))
        )
        (list (car pixels))
        (list)
    )
  )
)

;Dom: r (number), g (number), b (number)
;Rec: hex (string)
;Desc: Pasa a traves de todos los elementos RGB y le calcula el cuociente y resto al dividirlo por 16, para asi asignarle
;      un valor exadecimal ingresando a ese indice en la lista (que se encuentra ordenada)
;Recursion: No usa
(define (RGB->hex r g b)
  (string-join 
    (list
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient r 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder r 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient g 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder g 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (quotient b 16))
      (list-ref (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (remainder b 16))
    )
  ""
  )
)

;Dom: pixels (list)
;Rec: pixels (list)
;Desc: Usando la funcion RGB->hex convierte los valores RGB en Hexadecimales y construye un TDA pixhex-d
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-rgb->hex pixels)
  (if (not (null? (cdr pixels)))
    (append (list (pixhex-d (first (car pixels)) (second (car pixels)) (RGB->hex (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) (sixth (car pixels)))) (recursion-rgb->hex (cdr pixels)))
    (list (pixhex-d (first (car pixels)) (second (car pixels)) (RGB->hex (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) (sixth (car pixels))))
  )
)

;Dom: pixels (list)
;Rec: pixels (list)
;Desc: Realiza una rotacion en 90 grados respecto al origen imaginando un plano cartesiano, o sea (x, y) -> (y, -x)
;      y le aplica una traslacion (0, width) para que el punto inferior izquierdo sea (1,1) y no un negativo
;      se llama nuevamente para que vaya modificando los siguientes elementos y con la funcion append se agregan.
;Recursion: Natural
(define (recursion-rotate90 pixels width)
  (if (not (null? (cdr pixels)))
    (append (list (append (list (second (car pixels)) (+ (* (first (car pixels)) -1) width 1)) (cddar pixels))) (recursion-rotate90 (cdr pixels) width))
    (list (append (list (second (car pixels)) (+ (* (first (car pixels)) -1) width 1)) (cddar pixels)))
  )
)

;Dom: pixels (list), color (bit? | hex?)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funcion eq? revisa si el pixel es del mismo color que el indicado por el histograma
;      si ese es el caso lo omite de la lista de pixeles,
;      y si no los salta (o sea que se mantiene agregado) y sigue recursivamente con el siguiente.
;Recursion: Natural
(define (recursion-compress pixels color)
  (if (not (null? (cdr pixels)))
    (if (not (eq? (third (car pixels)) color))
        (append (list (car pixels)) (recursion-compress (cdr pixels) color))
        (append (list) (recursion-compress (cdr pixels) color))
    )
    (if (not (eq? (third (car pixels)) color))
        (list (car pixels))
        (list)
    )
  )
)

;Dom: pixels (list), color (rgb?)
;Rec: pixels (list)
;Desc: Recursivamente a traves de las funcion eq-rgb? revisa si el pixel es del mismo color que el indicado por el histograma
;      si ese es el caso lo omite de la lista de pixeles,
;      y si no los salta (o sea que se mantiene agregado) y sigue recursivamente con el siguiente.
;Recursion: Natural
;NOTA: Es el mismo algoritmo de arriba, solamente que este funciona para pixmaps (ya que la comparacion de rgb? se trata de distinta forma)
(define (recursion-compress-rgb pixels color)
  (if (not (null? (cdr pixels)))
    (if (not (eq-rgb? (list (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) color))
        (append (list (car pixels)) (recursion-compress-rgb (cdr pixels) color))
        (append (list) (recursion-compress-rgb (cdr pixels) color))
    )
    (if (not (eq-rgb? (list (third (car pixels)) (fourth (car pixels)) (fifth (car pixels))) color))
        (list (car pixels))
        (list)
    )
  )
)

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
  (sort
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
    #:key cadr >
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

; Exportar las funciones del TDA
(provide (all-defined-out))