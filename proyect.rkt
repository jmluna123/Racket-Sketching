#lang sketching

;program variable
(define object-size 20)
(define object-color "#000000")
(define object-type #\c)
(define count 0)

; memmory data
(define n 2000)
(define mem-x (make-vector n 0.0))
(define mem-y (make-vector n 0.0))
(define mem-sizes (make-vector n 0.0))
(define mem-colors (make-vector n "#000000"))
(define mem-types (make-vector n #\a))

(define (setup)
  (size 840 560)
  (frame-rate 60))
 
(define (draw)
  (background 255)
  (no-stroke)

  (cond
    [(mouse-pressed)
     (men-figure mouse-x mouse-y object-size object-color object-type)])

  (for ([i count])
    (draw-figure (mem-x.ref i) (mem-y.ref i) (mem-sizes.ref i) (mem-colors.ref i) (mem-types.ref i)))

  (draw-figure mouse-x mouse-y object-size object-color object-type)
  )


;------- draw figures ------- 
(define (draw-circle x y size color)
  (fill color)
  (circle x y size))

(define (draw-rect x y size color)
  (fill color)
  (rect x y size size))

(define (draw-figure x y size color type)
  (cond
    [(char=? type #\c)
     (draw-circle x y size color)]
    [(char=? type #\r)
     (draw-rect x y size color)])
  )


;------- save figures ------- 

(define (men-figure x y size color type)
  (:= mem-x count x)
  (:= mem-y count y)
  (:= mem-sizes count size)
  (:= mem-colors count color)
  (:= mem-types count type)
  
  (:= count (modulo (++ count) n))
  )

;----







(define (on-key-pressed)
  (define k key)
  (cond
    [(equal? k 'add)
     (:= object-size (+ object-size 5))]
    [(and (> object-size 5) (equal? k 'subtract))
     (:= object-size (- object-size 5))]
    
    [(and (char? k) (char=? k #\a))
     (:= object-color "#3232FF")]
    [(and (char? k) (char=? k #\d))
     (:= object-color "#FF3636")]
    [(and (char? k) (char=? k #\f))
     (:= object-color "#88FF36")]
    
    [(and (char? k) (char=? k #\c))
     (:= object-type k)]
    [(and (char? k) (char=? k #\r))
     (:= object-type k)]

    [(and (char? k) (char=? k #\s))
     (for ([i count])
    (draw-figure (mem-x.ref i) (mem-y.ref i) (mem-sizes.ref i) (mem-colors.ref i) (mem-types.ref i)))
     (save "C:/projects/Racket-Sketching/test.png")]
    )
  )