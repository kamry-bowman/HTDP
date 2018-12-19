;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 495|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


; [List-of Number] -> Number
; Sums up the list of numbers
(check-expect (sum.v1 '(1 2 3)) 6)
(define (sum.v1 alon)
  (cond
    [(empty? alon) 0]
    [else (+ (first alon) (sum.v1 (rest alon)))]))

; [List-of Number] -> Number
; Sums up the list of numbers
(check-expect (sum.v2 '(1 2 3)) 6)
(define (sum.v2 alon0)
  (local (; [List-of Number] ??? -> Number
          ; computes the sum of the numbers on alon
          ; accumulator: a is the sum of the numbers
          ; that alon lacks from alon0
          (define (sum/a alon a)
            (cond
              [(empty? alon) a]
              [else (sum/a (rest alon)
                            (+ (first alon) a))])))
    (sum/a alon0 0)))

(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

(define (!.v2 n0)
  (local (; N ??? -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator: a is the product of the
          ; natural numbers in the interval [n0, n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))