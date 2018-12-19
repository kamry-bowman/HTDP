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

(check-expect (product.v0 '(2 2 3 5)) 60)
(define (product.v0 lon)
  (cond
    [(empty? lon) 1]
    [else (* (first lon) (product.v0 (rest lon)))]))


(define (product.v1 lon0)
  (local (; [List-of Number] Number -> Number
          ; Processes a lon, determines their product
          ; accumulator: represents the product of all
          ; numbers in lon0 not in lon0
          (define (product/a lon a)
            (cond
              [(empty? lon) a]
              [else (product/a (rest lon) (* (first lon) a))])))
    (product/a lon0 1)))
(check-expect (product.v1 '(2 2 3 5)) 60)

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n0)
  (local (; Number Number -> Number
          ; Adds n to pi without using +
          ; accumulator: represents sum
          ; of numbers in n0 not in n plus pi
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi/a (sub1 n) (add1 a))])))
    (add-to-pi/a n0 pi)))

; [List-of X] -> [List-of X]
; consumes a list of X, returns the list as a palindrome
; with the last item in the input as the middle item in the
; return value, and the items up the final item in the input following
; the middle item but in reverse order

(define (palindrome lox0)
  (local (;[List-of X] [List-of X] -> [List-of X]
          ; accumulator: a is a list of items in lox0
          ; not in lox
          (define (palindrome/a lox a)
            (cond [(empty? lox) '()]
                  [(empty? (rest lox)) (append lox0 a)]
                  [else (palindrome/a (rest lox) (cons (first lox) a))])))
    (palindrome/a lox0 '())))

(check-expect (palindrome (explode "abcd")) (explode "abcdcba"))
(palindrome (explode "abcd"))
    
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate-until.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate-until.v2 '((0 4 5) (0 2 3) (0 3 5))) "all start with 0")
(define (rotate-until.v2 M0)
  (local (; Matrix Matrix -> Matrix
          ; accumulator a represents the unexamined items
          ; from the original M0
          (define (rotate/a M a)
            (cond
              [(not (= (first (first M)) 0)) M]
              [(empty? a) (error "all start with 0")]
              [else
               (rotate/a (append (rest M) (list (first M))) (rest a))])))
    (rotate/a M0 M0)))





