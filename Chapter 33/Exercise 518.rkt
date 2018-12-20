;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 518|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct pair [left right])
; ConsOrEmpty is one of:
; -'()
; -(make-pair Any ConsOrEmpty)

; Any ConsOrEmpty -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "our-cons: ...")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(define (our-first mimicked-list)
  (if (empty? mimicked-list)
      (error "our-first: ...")
      (pair-left mimicked-list)))

; ConsOrEmpty -> Any
; extracts the right part of the given pair
(define (our-rest mimicked-list)
  (local ((define right (pair-right
                         mimicked-list)))
  (cond
    [(empty? right) right]
    [else (our-cons (our-first right) (our-rest right))])))

(define test (our-cons 1 (our-cons 2 (our-cons 3 '()))))
(our-first test)
(our-rest test)
