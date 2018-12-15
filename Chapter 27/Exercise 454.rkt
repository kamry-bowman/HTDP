;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 454|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number [List-of Number] -> [List-of [List-of Number]]
; Consumes a number n and a list of n^2 numbers, and returns
; a n x n matrix.
(check-expect (create-matrix 2 (list 1 2 3 4)) '((1 2) (3 4)))
; (check-expect (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
;                             '((1 2 3) (4 5 6) (7 8 9)))
(define (create-matrix n lon)
  (local ((define (build-matrix lon)
            (cond
              [(empty? lon) '()]
              [else (local
                      ((define (split n lon row)
                        (cond
                          [(= n 0) (list (reverse row) lon)]
                          [else (split (sub1 n) (rest lon) (cons (first lon) row))]))
                      (define row-rest
                      (split n lon '())))
                      (cons (first row-rest)
                  (build-matrix (second row-rest))))])))
    (build-matrix lon)))