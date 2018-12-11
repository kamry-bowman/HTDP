;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 395|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] Natural -> [List-of X]
(define (take l n)
  (cond
    [(= n 0) '()]
    [(empty? l) (error "list is short")]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

(check-expect (take '() 0) '())
(check-expect (take '(1) 1) '(1))
(check-expect (take '(a b) 1) '(a))
(check-expect (take '(a b) 2) '(a b))
(check-error (take '() 1) "list is short")