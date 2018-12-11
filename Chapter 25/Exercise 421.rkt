;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 421|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] N -> [List-of [List-of X]]
; takes a list of X and bundles it into a
; list of lists of X of length N
(define (list->chunk l n)
  (cond
    [(empty? l) '()]
    [(= n 0) (error "Chunks must be greater than 0")]
    [else (cons (take l n) (list->chunk (drop l n) n))]))
(check-expect (list->chunk '() 0) '())
(check-expect (list->chunk '() 1) '())
(check-error (list->chunk '(a) 0) "Chunks must be greater than 0")
(check-expect (list->chunk '(a b c) 1) '((a) (b) (c)))
(check-expect (list->chunk '(a b c) 2) '((a b) (c)))

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (map implode (list->chunk s n)))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

(check-expect (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))

; String -> [List-of String]
(define (partition s n)
  (bundle (explode s) n))
(check-expect (partition "h" 1) '("h"))
(check-expect (partition "hello" 2) '("he" "ll" "o"))

