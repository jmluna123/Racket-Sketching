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
     (:= object-type #\r)]
    [(isin-black-color? mouse-x mouse-y)
     (:= object-color color-black)]
    [(isin-white-color? mouse-x mouse-y)
     (:= object-color color-white)]
    [(isin-red-color? mouse-x mouse-y)
     (:= object-color color-red)]
    [(isin-peach-color? mouse-x mouse-y)
     (:= object-color color-peach)]
    [(isin-purple-color? mouse-x mouse-y)
     (:= object-color color-purple)]
    [(isin-gray-color? mouse-x mouse-y)
     (:= object-color color-gray)]
    [(isin-yellow-color? mouse-x mouse-y)
     (:= object-color color-yellow)]
    [(isin-orange-color? mouse-x mouse-y)
     (:= object-color color-orange)]
    [(isin-green-color? mouse-x mouse-y)
     (:= object-color color-green)]
    [(isin-lime-color? mouse-x mouse-y)
     (:= object-color color-lime)]
    [(isin-blue-color? mouse-x mouse-y)
     (:= object-color color-blue)]
    [(isin-light-color? mouse-x mouse-y)
     (:= object-color color-light)])
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
(define stroke-color 160)

(define (draw-color-menu)
  ; separación
  (stroke 200)
  (fill 255)
  (rect color-menu-bx 0 2000 90)
  (rect color-menu-bx 0 color-menu-size 90)
  ;titulo
  (fill 0)
  (text "Colores" 295 5)
  ;colores
  (color-selected)
  (draw-black-color)
  (draw-white-color)
  (draw-red-color)
  (draw-peach-color)
  (draw-purple-color)
  (draw-gray-color)
  (draw-yellow-color)
  (draw-orange-color)
  (draw-green-color)
  (draw-lime-color)
  (draw-blue-color)
  (draw-light-color)
  )

(define color-slt-hd 33)
(define color-slt-rd 25)

(define (color-selected)
  (stroke stroke-color)
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
  (stroke stroke-color)
  (fill color-black)
  (circle (+ bx-black colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-black-color? x y)
  (and (< bx-black x (+ bx-black (* 2 colors-rd)))
       (< by-black y  (+ by-black (* 2 colors-rd))))
  )

;WHITE
(define bx-white (+ colors-lat (* colors-clm-size 0)))
(define by-white (- by-center-2 colors-rd))
(define color-white "#FFFFFF")

(define (draw-white-color)
  (stroke stroke-color)
  (fill color-white)
  (circle (+ bx-white colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-white-color? x y)
  (and (< bx-white x (+ bx-white (* 2 colors-rd)))
       (< by-white y  (+ by-white (* 2 colors-rd))))
  )

;RED
(define bx-red (+ colors-lat (* colors-clm-size 1)))
(define by-red (- by-center-1 colors-rd))
(define color-red "#ED1C24")

(define (draw-red-color)
  (stroke stroke-color)
  (fill color-red)
  (circle (+ bx-red colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-red-color? x y)
  (and (< bx-red x (+ bx-red (* 2 colors-rd)))
       (< by-red y  (+ by-red (* 2 colors-rd))))
  )

;PEACH
(define bx-peach (+ colors-lat (* colors-clm-size 1)))
(define by-peach (- by-center-2 colors-rd))
(define color-peach "#EFE4B0")

(define (draw-peach-color)
  (stroke stroke-color)
  (fill color-peach)
  (circle (+ bx-peach colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-peach-color? x y)
  (and (< bx-peach x (+ bx-peach (* 2 colors-rd)))
       (< by-peach y  (+ by-peach (* 2 colors-rd))))
  )

;PURPLE
(define bx-purple (+ colors-lat (* colors-clm-size 2)))
(define by-purple (- by-center-1 colors-rd))
(define color-purple "#A349A4")

(define (draw-purple-color)
  (stroke stroke-color)
  (fill color-purple)
  (circle (+ bx-purple colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-purple-color? x y)
  (and (< bx-purple x (+ bx-purple (* 2 colors-rd)))
       (< by-purple y  (+ by-purple (* 2 colors-rd))))
  )

;GRAY
(define bx-gray (+ colors-lat (* colors-clm-size 2)))
(define by-gray (- by-center-2 colors-rd))
(define color-gray "#C3C3C3")

(define (draw-gray-color)
  (stroke stroke-color)
  (fill color-gray)
  (circle (+ bx-gray colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-gray-color? x y)
  (and (< bx-gray x (+ bx-gray (* 2 colors-rd)))
       (< by-gray y  (+ by-gray (* 2 colors-rd))))
  )

;YELLOW
(define bx-yellow (+ colors-lat (* colors-clm-size 3)))
(define by-yellow (- by-center-1 colors-rd))
(define color-yellow "#FFF200")

(define (draw-yellow-color)
  (stroke stroke-color)
  (fill color-yellow)
  (circle (+ bx-yellow colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-yellow-color? x y)
  (and (< bx-yellow x (+ bx-yellow (* 2 colors-rd)))
       (< by-yellow y  (+ by-yellow (* 2 colors-rd))))
  )

;ORANGE
(define bx-orange (+ colors-lat (* colors-clm-size 3)))
(define by-orange (- by-center-2 colors-rd))
(define color-orange "#FF7F27")

(define (draw-orange-color)
  (stroke stroke-color)
  (fill color-orange)
  (circle (+ bx-orange colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-orange-color? x y)
  (and (< bx-orange x (+ bx-orange (* 2 colors-rd)))
       (< by-orange y  (+ by-orange (* 2 colors-rd))))
  )

;GREEN
(define bx-green (+ colors-lat (* colors-clm-size 4)))
(define by-green (- by-center-1 colors-rd))
(define color-green "#22B14C")

(define (draw-green-color)
  (stroke stroke-color)
  (fill color-green)
  (circle (+ bx-green colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-green-color? x y)
  (and (< bx-green x (+ bx-green (* 2 colors-rd)))
       (< by-green y  (+ by-green (* 2 colors-rd))))
  )

;LIME
(define bx-lime (+ colors-lat (* colors-clm-size 4)))
(define by-lime (- by-center-2 colors-rd))
(define color-lime "#B5E61D")

(define (draw-lime-color)
  (stroke stroke-color)
  (fill color-lime)
  (circle (+ bx-lime colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-lime-color? x y)
  (and (< bx-lime x (+ bx-lime (* 2 colors-rd)))
       (< by-lime y  (+ by-lime (* 2 colors-rd))))
  )

;BLUE
(define bx-blue (+ colors-lat (* colors-clm-size 5)))
(define by-blue (- by-center-1 colors-rd))
(define color-blue "#3F48CC")

(define (draw-blue-color)
  (stroke stroke-color)
  (fill color-blue)
  (circle (+ bx-blue colors-rd) by-center-1 (* 2 colors-rd)))

(define (isin-blue-color? x y)
  (and (< bx-blue x (+ bx-blue (* 2 colors-rd)))
       (< by-blue y  (+ by-blue (* 2 colors-rd))))
  )

;LIGHT
(define bx-light (+ colors-lat (* colors-clm-size 5)))
(define by-light (- by-center-2 colors-rd))
(define color-light "#99D9EA")

(define (draw-light-color)
  (stroke stroke-color)
  (fill color-light)
  (circle (+ bx-light colors-rd) by-center-2 (* 2 colors-rd)))

(define (isin-light-color? x y)
  (and (< bx-light x (+ bx-light (* 2 colors-rd)))
       (< by-light y  (+ by-light (* 2 colors-rd))))
  )


(define (on-key-pressed)
  (define k key)
  (cond
    [(equal? k 'add)
     (:= object-size (+ object-size 5))]
    [(and (> object-size 5) (equal? k 'subtract))
     (:= object-size (- object-size 5))]
    )
  )
