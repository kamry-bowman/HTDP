;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 458|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon .1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between
; a and b
; assume (< a b) holds
(define (integrate f left right)
  (integrate-kepler f left right))

(define (constant x) 20)
(check-within (integrate constant 12 22) 200 epsilon)

(define (linear x) (* 2 x))
(check-within (integrate linear 0 10) 100 epsilon)

(define (square x) (* 3 (sqr x)))
(check-within (integrate square 0 10)
              (- (expt 10 3) (expt 0 3))
              epsilon)

(define (integrate-kepler f l r)
  (local ((define f@r (f r))
          (define f@l (f l)))
    (+
     (* (- r l) f@r)
     (* .5 (- r l) (- f@l f@r)))))
  