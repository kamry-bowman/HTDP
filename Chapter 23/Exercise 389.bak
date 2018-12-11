;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 389|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on 
; hours and wages/h 
; assume the two lists are of equal length 
(define (wages*.v2 hours wages/h)
  (cond
    [(empty? hours) '()]
    [else
     (cons (* (first hours) (first wages/h))
           (wages*.v2 (rest hours) (rest wages/h)))]))

(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 (list 5.65) (list 40))
              (list 226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))


(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;    (make-phone-record String String)
(define names (list "john" "fred" "ralph"))
(define phones (list "111" "222" "333"))

(define (zip* proc list1 list2)
  (cond [(empty? list1) '()]
        [else (cons (proc (first list1) (first list2)) (zip* proc (rest list1) (rest list2)))]))

; [List-of String] [List-of Number] -> [List-of PhoneRecord]
(define (zip-phones list1 list2)
  (zip* make-phone-record list1 list2))

(check-expect (zip-phones names phones) (list (make-phone-record "john" "111") (make-phone-record "fred" "222") (make-phone-record "ralph" "333")))
