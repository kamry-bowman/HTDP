;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 346|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; A BSL-expr is one of:
; BSL-value
; (operator BSL-expr BSL-expr)

; BSL-value
; is a
; Number

(define-struct add [left right])
(define-struct mul [left right])

(define (eval-expression expr)
  (match expr
    [(? number?) expr]
    [(add left right)
     (+ (eval-expression left) (eval-expression right))]
    [(mul left right)
     (* (eval-expression left) (eval-expression right))]))


    
    