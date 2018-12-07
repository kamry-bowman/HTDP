;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Build-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;[x] N [x] [N -> x] -> [list-of x]
(check-expect (build-l*st 5 add1) (list 1 2 3 4 5))
(define (build-l*st n f)
  (reverse (build-rev-l*st n f)))

(define (build-rev-l*st n f)
  (cond [(= n 0) '()]
                 [else (cons (f (- n 1))
                             (build-rev-l*st (- n 1) f))]))
  
  