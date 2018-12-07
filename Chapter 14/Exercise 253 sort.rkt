;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 253 sort|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; Sorts a list of X progressively using comparison function


;(define num-list '(3 4 1 2 5))
;(define num-list-sorted '(5 4 3 2 1))

(check-expect (sort-l '(3 4) >) '(4 3))
(check-expect (sort-l '(5 22 3 4) >) '(22 5 4 3))

(define (sort-l l f)
  (cond [(empty? l) '()]
        [else (insert (first l) (sort-l (rest l) f) f)]))

;(check-expect (insert 4 '(9 5 3) >) '(9 5 4 3))
;[x][x][list-of x][x x -> Boolean] -> [list-of x]
(define (insert x l f)
  (cond [(empty? l) (cons x '())]
        [else (if (f x (first l))
                  (cons x l)
                  (cons (first l) (insert x (rest l) f)))]))