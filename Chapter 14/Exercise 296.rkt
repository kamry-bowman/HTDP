;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 296|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a Set is a function:
; [List-of X -> Boolean]
; interpretation a set denotes all elements that are described by a given set function.

; [X Set -> Boolean
(define (in-set? x s)
  (s x))

; Even set
(define all-evens
  (lambda (n)
    (and (number? n)
         (= (modulo n 2) 0))))

(check-expect (in-set? 2 all-evens) #true)
(check-expect (in-set? 3 all-evens) #false)

; Set X -> Set
; Adds an element X to a set
(define (add-element set x)
  (lambda (y)
    (or (set y)
        (= y x))))

; Set Set -> Set
; Creates