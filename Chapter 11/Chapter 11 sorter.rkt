;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Chapter 11 sorter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List -> Boolean
; Accepts a number list and returns true if it's sorted greater than lesser
; left to right
(define (sorted>? l)
  (cond [(empty? l) #true]
        [(empty? (rest l)) #true]
        [else (and (>= (first l) (first (rest l)))
                   (sorted>? (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon
(check-satisfied (insert 5 '()) sorted>?)
(check-satisfied (insert 5 (list 6)) sorted>?)
(check-satisfied (insert 5 (list 4)) sorted>?)
(check-satisfied (insert 12 (list 20 -5)) sorted>?)
(define (insert n alon)
  (cond [(empty? alon) (list n)]
        [(cons? alon) (if (>= n (first alon))
                          (cons n alon)
                          (cons (first alon) (insert n (rest alon))))]))


; List-of-numbers -> List-of-numbers 
; produces a sorted version of alon
(check-satisfied (sort> '()) sorted>?)
(check-satisfied (sort> (cons 12 (cons 20 (cons -5 '())))) sorted>?)
(define (sort> alon)
  (cond [(empty? alon) '()]
        [(cons? alon) (insert (first alon) (sort> (rest alon)))]))

(sort-function?

(check-satisfied (sort>/bad 5) sort-function?)

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l) 
  (list 9 8 7 6 5 4 3 2 1 0))
