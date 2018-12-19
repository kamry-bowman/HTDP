;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 499|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct node [left right])
; A Tree is one of:
; - '()
; - (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree -> N
; measures the height of abt0

(define (height abt)
  (local (; Tree N N -> N
          ; measures the height of abt
          ; accumulator s: is the number of steps 
          ; it takes to reach abt from abt0
          ; accumulator m: is the maximal height of
          ; the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) (max s m)]
              [else
               (h/a (node-left abt)
                     (add1 s)
                     (h/a (node-right abt)
                          (add1 s)
                          0))])))
    (h/a abt 0 0)))

(check-expect (height example) 3)


