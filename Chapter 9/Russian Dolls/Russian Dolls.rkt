;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Russian Dolls|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct layer [color doll])

; An RD (short for Russian doll) is one of:
;  -String
;  -(make-layer String RD)

(make-layer "yellow" (make-layer "green" "red"))

; RD -> Number
; how many dolls are part of an-rd
;(check-expect (depth "red") 1)
;(check-expect (depth (make-layer "yellow" (make-layer "green" "red"))) 3)

(define (depth an-rd)
  (cond [(string? an-rd) 1]
        [else (+ (depth (layer-doll an-rd)) 1)]))

; RD -> string
; Consumes a russian doll, returns a string of each color, separated by a comma
;(check-expect (colors (make-layer "orange" (make-layer "red" (make-layer "red" (make-layer "green" "yellow"))))) "orange, red, red, green, yellow")

(define (colors rd)
  (cond [(string? rd) rd]
        [(layer? rd) (string-append (layer-color rd) ", " (colors (layer-doll rd)))]))

;RD -> string
;Consumes a russian doll, returns the innermost color string
(check-expect (inner (make-layer "orange" (make-layer "red" (make-layer "red" (make-layer "green" "yellow"))))) "yellow")
(define (inner rd)
  (cond [(string? rd) rd]
        [(layer? rd) (inner (layer-doll rd))]))
  
