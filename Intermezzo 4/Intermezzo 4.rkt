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

; Number Number -> (list Number Number)
; Accepts a mantissa and an exponent. Divides mantissa by 10
; and increases exponent by 1 until no longer possible
(define (transfer-excess m e)
            (if (and (< e 99) (= (floor (/ m 10)) (/ m 10)))
                (transfer-excess (floor (/ m 10)) (+ e 1))
                (list m e)))
(check-expect (transfer-excess 5 0) '(5 0))
(check-expect (transfer-excess 20 0) '(2 1))
(check-expect (transfer-excess 21 0) '(21 0))
(check-expect (transfer-excess 105 2) '(105 2))

; Inex Inex => Inex
(define (inex+ a b)
  (local ((define sum_result
            (+ (* (inex-sign a) (inex-mantissa a) (expt 10 (inex-exponent a)))
               (* (inex-sign b) (inex-mantissa b) (expt 10 (inex-exponent b)))))
          (define (split-sign n)
            (if (>= n 0)
                (list n 1)
                `(,(* -1 n) -1)))
          (define m-e
            (transfer-excess sum_result 0))
          (define exponent (second m-e))
          (define m-s (split-sign (first m-e)))
          (define mantissa (first m-s))
          (define sign (second m-s)))
    (if (validate-inex mantissa sign exponent)
        (create-inex mantissa sign exponent)
        (error "invalid result"))))

(check-expect (inex+ (create-inex 5 1 10) (create-inex 1 1 10)) (create-inex 6 1 10))
(check-expect (inex+ (create-inex 5 1 10) (create-inex 6 1 10)) (create-inex 11 1 10))
(check-expect (inex+ (create-inex 5 1 2) (create-inex 6 1 3)) (create-inex 65 1 2))
(check-expect (inex+ (create-inex 5 1 12) (create-inex 5 -1 11)) (create-inex 45 1 11))
(check-expect (inex+ (create-inex 5 -1 12) (create-inex 5 1 11)) (create-inex 45 -1 11))
