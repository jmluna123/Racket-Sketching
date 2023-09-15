#lang sketching

(define (setup)
  (size 100 100)
  ; Use no-loop to produce one frame only
  (no-loop))
 
(define (draw)
  (background "white")
  (stroke "red")
  (line 0 0 100 100)
  (stroke "blue")
  (line 0 100 100 0)
  (save "test.png"))