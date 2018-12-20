;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 525|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image Posn Posn Posn -> Image
; generative adds the triangle (a, b ,c) to s,
; subdivides it into three triangles by taking the
; midpoints of its sides; stop if (a, b, c) is too small
;accumulator the function accumulates the triangles scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b))
        (define scene4
          (add-sierpinski scene3 c mid-c-a mid-b-c)))
        ; -IN-
        (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; Image Posn Posn Posn
(define (add-triangle scene a b c)
  (add-polygon scene (list a b c) 'outline 'black))

; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (local ((define (test-two p1 p2)
            (< (sqrt (+ (expt (- (posn-x p1) (posn-x p2)) 2)
                         (expt (- (posn-y p1) (posn-y p2)) 2)))
               5)))
    (or (test-two a b)
         (test-two b c)
         (test-two c a))))

; Posn Posn -> Posn 
; determines the midpoint between a and b
(define (mid-point a b)
  (make-posn (* .5 (+ (posn-x a) (posn-x b)))
             (* .5 (+ (posn-y a) (posn-y b)))))

(define MT (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))
 
(add-sierpinski MT A B C)