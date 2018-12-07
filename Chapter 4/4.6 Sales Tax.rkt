;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4.6 Sales Tax|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 12017) (* 0.08 12017))
(check-expect (sales-tax 10000) (* 0.08 10000))
(check-expect (sales-tax 1500) (* .05 1500))

(define price-bound1 1000)
(define price-bound2 10000)


; Price -> Number
; computes the amount of tax charged for p
(define (sales-tax p)
  (cond
    [(< p price-bound1) 0]
    [(< p price-bound2) (* p .05)]
    [(>= p price-bound2) (* p .08)]))
