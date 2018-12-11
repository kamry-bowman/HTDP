;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Intermezzo 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
       10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(= (inex->number (create-inex 50 -1 20)) (inex->number (create-inex 5 -1 19)))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

; Number Number Number -> Boolean
; Confirms that m s e would combine as mantissa
; sign and exponent for a valid inex structure
(define (validate-inex m s e)
  (and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1))))

; Inex Inex => Inex
(define (inex+ a b)
  (local ((define sum_result
            (+ (* (inex-sign a) (inex-mantissa a))
               (* (inex-sign b) (inex-mantissa b))))
          (define (transfer-excess m e)
            (if (< m 99)
                (list m e)
                (transfer-excess (floor (/ m 10)) (+ e 1))
          ))
          (define (split-sign n)
            (if (>= n 0)
                (list n 1)
                `(,(* -1 n) -1)))
          (define m-e
            (transfer-excess sum_result (inex-exponent a)))
          (define exponent (second m-e))
          (define m-s (split-sign (first m-e)))
          (define mantissa (first m-s))
          (define sign (second m-s)))
    (if (validate-inex mantissa sign exponent)
        (create-inex mantissa sign exponent)
        (error "invalid result"))))

(check-expect (inex+ (create-inex 5 1 10) (create-inex 1 1 10)) (create-inex 6 1 10))
(check-expect (inex+ (create-inex 5 1 10) (create-inex 6 1 10)) (create-inex 11 1 10))
