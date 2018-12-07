;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 47|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;An NEList-of-Booleans is either
; - (cons Boolean '())
; - (cons Boolean NEList-of-Booleans)


;NEList-of-Booleans -> Boolean
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false (cons #true '()))) #false)

(define (all-true nlob)
  (cond [(empty? (rest nlob)) (first nlob)]
        [else (and (first nlob)
                   (all-true (rest nlob)))]))

(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #true '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #false (cons #false (cons #true (cons #false '()))))) #true)
(check-expect (one-true (cons #false (cons #false (cons #false (cons #false '()))))) #false)

(define (one-true nlob)
  (cond [(empty? (rest nlob)) (first nlob)]
        [else (or (first nlob)
                   (one-true (rest nlob)))]))

