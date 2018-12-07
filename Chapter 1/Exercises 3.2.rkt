;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercises 3.2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;String -> String
;Accepts a non-empty string and returns the first character as a string
;Given: "Mormon", Expect: "M"
;Given: "Foreman" Expect: "F"
(define (string-first s)
  (substring s 0 1))

;String -> String
;Accepts a non-empty string and returns the last character as a string
;Given: "Mormon", Expect: "n"
(define (string-last s)
  (substring s (- (string-length s) 1) (string-length s)))

;Image -> Number
;Number represents pixels
;Accepts an image, and returns the number of pixels making up the image
;Given: (rectangle 50 10 "solid" "black"), Expect: 500
(define (image-area img)
  (* (image-width img) (image-height img)))

;String -> String
;Accepts a string, and returns a string with all characters except the first
;Given: "Shaggoth", Expect: "haggoth"
(define (string-rest str)
  (substring str 1 (string-length str)))

;String -> String
;Accepts a string, returns the string with the last character removed
;Given: "Port-Au-Prince", Expect; "Port-Au-Princ"
(define (string-remove-last str)
  (substring str 0 (- (string-length  str) 1)))

; Number -> Number 
; converts Fahrenheit temperatures to Celsius
; given 32, expect 0
; given 212, expect 100
; given -40, expect -40
(define (f2c f)
  (* 5/9 (- f 32)))