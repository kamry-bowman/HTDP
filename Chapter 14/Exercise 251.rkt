;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 251|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
#|

; [List-of Number] [Number -> Number] -> Number
; computes all numbers of l combined by function f
(define (fold1 g l)
  (cond [(empty? l) 0]
        [(empty? (rest l)) (g (first l))]
        [else (g (first l) (fold1 g (rest l)))]))

; [List-of Number] -> Number
; computes all numbers of l summed together
(check-expect (sum-fold1 (list 5 3 2 1)) (+ 5 3 2 1))
(define (sum-fold1 l)
  (fold1 + l))

; [List-of Number] -> Number
; computes all numbers of p summed together
(check-expect (product-fold1 (list 5 3 2 1)) (* 5 3 2 1))
(define (product-fold1 l)
  (fold1 * l))
|#

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))
#|

; [List-of Posn) -> Image
(check-expect (image* '()) emt)
(check-expect (image* (list (make-posn 4 3))) (place-image dot 4 3 emt))
(check-expect (image* (list (make-posn 4 3) (make-posn 5 2))) (place-image dot 5 2 (place-image dot 4 3 emt)))
(define (image* l)
  (cond
    [(empty? l) emt]
    [else (place-dot (first l)
                     (image* (rest l)))]))
|#

; Posn Image -> Image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; [ITEM1 ITEM2] [List-of ITEM1] [ITEM1 ITEM2 -> ITEM2] ITEM 2 -> ITEM 2
; computes all ITEM1 using provided function, to create ITEM2.
; Provided an ITEM2 base case.
(define (fold2 f l bc)
  (cond [(empty? l) bc]
        [else (f (first l) (fold2 f (rest l) bc))]))

(check-expect (image*fold '()) emt)
(check-expect (image*fold (list (make-posn 4 3))) (place-image dot 4 3 emt))
(check-expect (image*fold (list (make-posn 4 3) (make-posn 5 2))) (place-image dot 5 2 (place-image dot 4 3 emt)))
(define (image*fold l)
  (fold2 place-dot l emt))

