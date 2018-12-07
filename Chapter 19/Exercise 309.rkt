;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 309|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; [List-of [List-of String]] -> [List-of Number]
; Consumes a List-of List-of Strings, and returns a List-of Numbers,
; with each Number representing the number of words in a given
; string
(check-expect (words-on-line '()) '())
(check-expect (words-on-line '(("munich"))) '(1))
(check-expect (words-on-line `(,'() ,'())) '(0 0))
(check-expect (words-on-line `(,'("touch" "me") ,'() ,'("me"))) '(2 0 1))
(define (words-on-line los)
  (local (;String -> Number
          ;Consumes a String, returns the number of words
          (define (words line)
            (match line
              ['() 0]
              [(cons head tail) (+ 1 (words tail))])))
    (match los
      ['() '()]
      [(cons head tail) (cons (words head) (words-on-line tail))])))