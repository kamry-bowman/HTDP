;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 306|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define (0-thru-minus-1 n)
  (for/list ([i n]) i))

(0-thru-minus-1 5)

(define (1-thru-n n)
  (for/list ([i n]) (+ i 1)))

(1-thru-n 5)

(define (first-n-evens n)
  (for/list ([i n]) (* 2 (+ i 1))))

(first-n-evens 5)

(define (identity-M n)
  (for/list ([i n])
    (for/list ([j n]) (if (= i j) 1 0))))

(define-struct phone [area switch four])

; List-of Phone -> List-of Phone
(define input `( ,(make-phone 713 111 1111) ,(make-phone 713 222 2222) ,(make-phone 333 333 3333)))
(define expect `( ,(make-phone 281 111 1111) ,(make-phone 281 222 2222) ,(make-phone 333 333 3333)))
(check-expect (replace input) expect)

(define (replace lop)
  (for/list ([p lop])
    (match p
      [(phone area switch four)
      (if (= area 713)
          (make-phone 281 switch four)
          (make-phone area switch four))])))