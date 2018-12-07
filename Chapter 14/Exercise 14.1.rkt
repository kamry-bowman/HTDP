;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 14.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; adds num to each item on l
(check-expect (addn-to-l 4 '(3 4)) '(7 8))
(define (addn-to-l n l)
  (cond [(empty? l) '()]
        [(cons? l) (cons (+ (first l) n)
                         (addn-to-l n (rest l)))]))