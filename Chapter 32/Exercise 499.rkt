;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 499|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct node [left right])
; A Tree is one of:
; - '()
; - (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt)))
             1)]))

(check-expect (height example) 3)
