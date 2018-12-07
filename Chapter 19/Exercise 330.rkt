;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 330|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

(define Text `(,"part1" ,"part2" ,"part3"))
(define Libs `((,"hang" ,"draw") (,"read!)")))
(define TS `(,"read!" ,Text ,Libs))

; Dir.v1 -> Number
; Determines number of File.v1 within Dir.v1 nested structure
(check-expect (how-many Text) 3)
(check-expect (how-many Libs) 3)
(check-expect (how-many TS) 7)
(define (how-many dir)
  (foldr (lambda (item accum)
         (if (string? item)
             (+ 1 accum)
             (+ (how-many item) accum)))
         0
         dir))