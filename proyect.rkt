#lang sketching

(define n 60)
(define elipse-size 2)
(define mx (make-vector n 0.0))
(define my (make-vector n 0.0))

(define c-base "#AA3232")
(define c-pressed "#FF3232")
 
(define (setup)
  (size 640 360)
  (frame-rate 60))
 
(define (draw)
  (background 51)
  (no-stroke)
  (if mouse-pressed  (fill c-pressed) (fill c-base))
 
  ; The arrays are used as circular buffers.
  ; This way we don't need to move anything.
  (define which (modulo frame-count n))
  (:= mx which mouse-x)
  (:= my which mouse-y)
 
  (for ([i n])
    ; which+1 is the index of the oldest entry
    (define index (modulo (+ which 1 i) n))
    (define tmp (* i elipse-size))
    (ellipse (mx.ref index) (my.ref index) tmp tmp)))

(define (on-key-pressed)
  (define k key)
  (cond
    [(equal? k 'add)
     (++ elipse-size)]
    [(char=? k #\a)
     (:= c-pressed "#3232FF")
     (:= c-base "#3232AA")]
    [(char=? k #\b)
     (:= c-pressed "#F4FA4A")
     (:= c-base "#A6AA32")]
    [(char=? k #\c)
     (:= c-pressed "#4AFAE5")
     (:= c-base "#22776D")]
    [else
     (:= c-base "#AA3232")
     (:= c-pressed "#FF3232")]
    )
  )