;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 160|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R

	
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define set123-version1
  (cons 1 (cons 2 (cons 3 '()))))
 
(define set123-version2
  (cons 1 (cons 3 (cons 2 '()))))

(define set23-version1
  (cons 2 (cons 3 '())))
 
(define set23-version2
  (cons 3 (cons 2 '())))



; Son
(define es '())
 
; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))
 
(check-expect
  (set-.L 1 s1.L) es)
 
(define (set-.L x s)
  (remove-all x s))

	
; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
 
(check-expect
  (set-.R 1 s1.R) es)
 
(define (set-.R x s)
  (remove x s))

; Number Son -> Son
; subtracts x from s
(define (set- x s)
  s)


; Son -> Boolean
; #true if 1 a member of s;  #false otherwise
(define (not-member-1? s)
  (not (in? 1 s)))

(check-satisfied (set-.L 1 set123-version1) not-member-1?)

(define (in-4? s)
  (in? 4 s))

(check-satisfied (set+.L 4 set123-version1) in-4?)
(define (set+.L x s)
  (cons x s))

(check-satisfied (set+.L 4 set123-version1) in-4?)
(define (set+.R x s)
  (if (not (in? x s))
      (cons x s)
      s))