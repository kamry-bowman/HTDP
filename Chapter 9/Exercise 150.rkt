;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 150|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N -> Number
; computes (+ n pi) without using +
 
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
 
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n) (add1 (add-to-pi (sub1 n)))]))

(check-expect (add 5 8) 13)
(define (add s n)
  (cond [(zero? n) s]
        [(positive? n) (add1 (add s (sub1 n)))]))

(check-expect (multiply 5 8) 40)
(define (multiply s n)
  (cond [(zero? n) 0]
        [(positive? n) (add s (multiply s (sub1 n)))]))