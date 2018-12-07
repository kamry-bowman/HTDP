;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 250|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> [List-of Number]
; tabules function f between n
; and 0 (incl.) in a list
(define (tab f n)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tab f (sub1 n)))]))

; Number -> [List-of Number]
; tabulates son between n and
; 0 (incl.) in a list
(check-within (tab-sin-f 3) (list (sin 3) (sin 2) (sin 1) (sin 0)) .00001)
(define (tab-sin-f n)
  (tab sin n))

; Number -> [List-of Number]
; tabulates son between n and
; 0 (incl.) in a list
(check-within (tab-sin 3) (list (sin 3) (sin 2) (sin 1) (sin 0)) .00001)
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(check-within (tab-sqrt 4) (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0)) .0001)
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(check-within (tab-sqrt-f 4) (list (sqrt 4) (sqrt 3) (sqrt 2) (sqrt 1) (sqrt 0)) .0001)
(define (tab-sqrt-f n)
  (tab sqrt n))