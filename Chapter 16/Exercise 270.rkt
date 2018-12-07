;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 270|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Build-list: [X] N [N->X] -> [List-of X]
;Builds up a list from 0 to n-1.
;N -> List-of N
(check-expect (list-to-n-1 6) '(0 1 2 3 4 5))
(define (list-to-n-1 n)
  (local (
          ; N -> N
          ; Returns number input
          (define (echo n)
            n))
    (build-list n echo)))


;Build-list: [X] N [N->X] -> [List-of X]
;Builds up a list from 0 to n-1.
;N -> List-of N
(check-expect (list-to-n 6) '(1 2 3 4 5 6))
(define (list-to-n n)
  (local (
          ; N -> N
          ; Returns number input
          (define (add-one n)
            (+ n 1)))
    (build-list n add-one)))


