;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 14.4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (function=at-1.2-3-and-5.775 f g)
  (and (= (f 1.2) (g 1.2))
       (= (f 3) (g 3))
       (= (f 5.775) (g 5.775))))

(define (f x) 0)
(define (g x) 0)

(function=at-1.2-3-and-5.775 f g)