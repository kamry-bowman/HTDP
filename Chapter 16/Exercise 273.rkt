;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 273|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;[x][y][List-of x][x -> y] -> [List-of y]
;Accepts a list, and performs a function on each item, returning
;a new list of the return values
(define (add2 n) (+ n 2))
(check-expect (map-from-fold add2 '(3 4 5)) '(5 6 7))
(define (map-from-fold f l)
  (local (; [x -> y] [list-of y] -> [list-of y]
          ; Performs f on l, and appends it to list
          (define (apply-and-cons x l)
            (cons (f x) l)))
    (foldr apply-and-cons '() l)))