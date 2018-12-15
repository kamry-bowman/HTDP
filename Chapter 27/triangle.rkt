;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname triangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define SMALL 4) ; a size measure in terms of pixels
(define small-triangle (triangle SMALL 'outline 'red))

; Number -> Image
; generative creates Sierpinksi tirangle of size side by
; gernerating one for (/ side 2) and placing one copy above two copies

(check-expect (sierpinski SMALL) small-triangle)
(check-expect (sierpinski (* 2 SMALL))
              (above small-triangle
                     (beside small-triangle small-triangle)))

(define (sierpinski side)
  (cond
    [(<= side SMALL) (triangle side 'outline 'red)]
    [else
     (local ((define half-sized (sierpinski (/ side 2))))
       (above half-sized (beside half-sized half-sized)))]))

(sierpinski (* SMALL 16))

