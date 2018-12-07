;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 140|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;  List-of-Booleans
;;Either
;; -'()
;; -(cons Boolean '())

;List-of-Booleans -> Boolean
; Reviews List-of-Booleans
; Returns false if there are any false items
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true (cons #true '())))) #true)
(check-expect (all-true (cons #true (cons #false (cons #true '())))) #false)

(define (all-true lb)
  (cond [(empty? lb) #true]
        [(cons? lb) (and (first lb)
                         (all-true (rest lb)))]))


;List-of-Booleans -> Boolean
; Reviews List-of-Booleans
; Returns true unless all items are false
(check-expect (one-true '()) #false)
(check-expect (one-true (cons  #true '())) #true)
(check-expect (one-true  (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)

(define (one-true lb)
  (cond [(empty? lb) #false]
        [(cons? lb) (or (first lb)
                         (one-true (rest lb)))]))
