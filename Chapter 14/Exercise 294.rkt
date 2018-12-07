;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 294|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> ([Maybe N] -> Boolean) 
(define (is-index x l)
  (lambda (maybe-n)
    (local (; N -> Boolean
                        (define (check-index maybe-n l)
                          (cond [(empty? l) (equal? maybe-n #false)]
                                [(equal? maybe-n 0) (equal? x (first l))]
                                [(equal? (first l) x) #false]
                                [else (check-index (- maybe-n 1) (rest l))])))
      (check-index maybe-n l))))