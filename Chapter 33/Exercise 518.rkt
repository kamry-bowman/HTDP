;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 518|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct cpair [count left right])
; A [MyList X] is one of:
; -'()
; - (make-cpair (tech "N") X [MyList X])
; accumulator the count field is the number of cpairs

; data defniitons, via a constructor-function
(define (our-cons f r)
  (cond
    [(empty? r) (make-cpair l f r)]
    [(cpair? r) (make-cpair (+ (cpair-count r) 1) f r)]
    [else (error "our-cons: ...")]))

; Any -> N
; how many items does l contain
(define (our-length l)
  (cond
    [(empty? l) 0]
    [(cpair? l) (cpair-count l)]
    [else (error "my-length: ...")]))