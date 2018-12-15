;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 455|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon .01)

; [Number -> Number] Number -> Number
; Consumes a function and an x value, and
; returns the slope of the line through
; `(,(- r1 epsilon) ,(f (- r1 epsilon)))
; and `(,(+ r1 epsilon) ,(f (+ r1 epsilon)))
(define (slope f r1)
  (local ((define r1- (- r1 epsilon))
          (define f@r1- (f r1-))
          (define r1+ (+ r1 epsilon))
          (define f@r1+ (f r1+)))
    (/ (- f@r1+ f@r1-)
       (- r1+ r1-))))

(define (linear1 x)
  (- 5 (* 2 x)))
(check-expect (slope linear1 2) -2)

(define (linear2 x)
  (+ 6 (* 3 x)))
(check-expect (slope linear2 3) 3)

(define (poly x)
  (* 2 (expt x 2)))
(check-expect (slope poly 1) 4)

; [Number -> Number] Number -> Number
; Consumes a function and an x value and determines
; the root of the tangent that passes through (r1, (f r1))
(define (root-of-tangent f r1)
  (- r1 (/ (f r1) (slope f r1))))

(check-expect (root-of-tangent linear1 3) 2.5)
(check-expect (root-of-tangent linear2 3) -2)
(check-expect (root-of-tangent poly 1) .5) 

  