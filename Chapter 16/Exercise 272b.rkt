;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 272b|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;[x][[List-of x][x x -> x] -> x]
;[List-of Number][Number Number -> Number] -> Number
;Takes a list
(check-expect (product-from-list '(3 4)) 12)
(check-expect (product-from-list '(3 4 2)) 24)
(define (product-from-list lon)
  (foldr * 1 lon))