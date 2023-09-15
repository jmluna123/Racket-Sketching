#lang sketching

;program variable
(define object-size 20)
(define object-color "#000000")
(define object-type #\c)
(define count 0)

; memmory data
(define n 10000)
(define mem-x (make-vector n 0.0))
(define mem-y (make-vector n 0.0))
(define mem-sizes (make-vector n 0.0))
(define mem-colors (make-vector n "#000000"))
(define mem-types (make-vector n #\a))

(define (setup)
  (size 940 660)
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
  (isin-buttons? mouse-x mouse-y)
  (draw-menu)
  )


;------- draw figures ------- 
(define (draw-circle x y size color)
  (fill color)
  (circle x y size))

(define (draw-rect x y size color)
  (fill color)
  (square x y size))

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

;-------------------- MENUS -------------------
(define menu-width 200)

(define (draw-menu)
  (draw-form-menu)
  (draw-color-menu)
  )

(define (isin-buttons? x y)
  (if (isin-button-circle? x y) (:= button-circle-fill 240) (:= button-circle-fill 255))
  (if (isin-button-rect? x y) (:= button-rect-fill 240) (:= button-rect-fill 255))
  )

(define (on-mouse-pressed)
  (cond
    [(isin-button-circle? mouse-x mouse-y)
     (:= button-circle-selected? #t)
     (:= button-rect-selected? #f)
     (:= object-type #\c)]
    [(isin-button-rect? mouse-x mouse-y)
     (:= button-rect-selected? #t)
     (:= button-circle-selected? #f)
     (:= object-type #\r)])
  )

;--------- MENU FORMAS -----------------
(define button-size 40)
(define button-smth 7)

(define form-lat 50)
(define form-head 40)


(define (draw-form-menu)
  ; separación
  (stroke 200)
  (fill 255)
  (rect 0 0 menu-width 90)
  ;titulo
  (fill 0)
  (text "Formas" 75 5)
  (button-circle)
  (button-rect)
  )

;- CIRCLE BUTTON
(define button-circle-selected? #t)
(define button-circle-fill 255)
(define button-circle-size button-size)
(define button-circle-bx 0)
(define button-circle-by 0)

(define (button-circle)
  (fill button-circle-fill)
  (if button-circle-selected? (stroke 0) (stroke 220))
  (:= button-circle-bx form-lat)
  (:= button-circle-by form-head)
  (rect button-circle-bx button-circle-by button-size button-size button-smth)
  (define center (/ button-size 2))
  (stroke 0)
  (circle (+ button-circle-bx center) (+ button-circle-by center) 24)
  )

(define (isin-button-circle? x y)
  (and (< button-circle-bx x (+ button-circle-bx button-circle-size))
       (< button-circle-by y (+ button-circle-by button-circle-size)))
  )

;- RECT BUTTON
(define button-rect-selected? #f)
(define button-rect-fill 255)
(define button-rect-size button-size)
(define button-rect-bx 0)
(define button-rect-by 0)

(define (button-rect)
  (fill button-rect-fill)
  (if button-rect-selected? (stroke 0) (stroke 220))
  (:= button-rect-bx (- (- menu-width form-lat) button-size))
  (:= button-rect-by form-head)
  (rect  button-rect-bx button-rect-by button-size button-size button-smth)
  (stroke 0)
  (define inter-size 22)
  (define pad (/ (- button-size inter-size) 2))
  (square (+ pad button-rect-bx) (+ pad button-rect-by) inter-size)
  )

(define (isin-button-rect? x y)
  (and (< button-rect-bx x (+ button-rect-bx button-rect-size))
       (< button-rect-by y (+ button-rect-by button-rect-size)))
  )

;--------- MENU COLORES -----------------
(define color-size 10)
(define color-menu-bx 200)
(define color-menu-size 250)

(define (draw-color-menu)
  ; separación
  (stroke 200)
  (fill 255)
  (rect color-menu-bx 0 color-menu-size 90)
  ;titulo
  (fill 0)
  (text "Colores" 295 5)
  ;colores
  (color-selected)
  (draw-black-color)
  (draw-white-color)
  )

(define color-slt-hd 33)
(define color-slt-rd 25)

(define (color-selected)
  (stroke 0)
  (fill object-color)
  (circle (+ color-menu-bx 50) (+ color-slt-hd color-slt-rd) (* color-slt-rd 2))
  )

(define colors-rd 10)
(define colors-lat (+ 235 (* color-slt-rd 2)))
(define by-center-1 (+ colors-rd color-slt-hd))
(define by-center-2 (+ 40 color-slt-hd))
(define colors-clm-size 25)

; BLACK
(define bx-black (+ colors-lat (* colors-clm-size 0)))
(define by-black (- by-center-1 colors-rd))
(define color-black "#000000")

(define (draw-black-color)
  (stroke 0)
  (fill color-black)
  (circle (+ colors-lat colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-black-color? x y)
  (and (< bx-black x (+ bx-black (* 2 colors-rd)))
       (< by-black y  (+ by-black (* 2 colors-rd))))
  )

;WHITE
(define bx-white (+ colors-lat (* colors-clm-size 0)))
(define by-white (- by-center-1 colors-rd))
(define color-white "#FFFFFF")

(define (draw-white-color)
  (stroke 0)
  (fill color-white)
  (circle (+ colors-lat colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-white-color? x y)
  (and (< bx-white x (+ bx-white (* 2 colors-rd)))
       (< by-white y  (+ by-white (* 2 colors-rd))))
  )
















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