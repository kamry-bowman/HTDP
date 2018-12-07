;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |UFO Exercise|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
 (require 2htdp/universe)
; A Vel is a structure
;; (make-vel Number Number)
;; interpretation: (make-vel deltax deltay) creates
;; a velocity structure with deltax and deltay
;; representing directional movement on x and y axes
(define-struct vel [deltax deltay])

 


 


(define-struct ufo [loc vel])
; A UFO is a structure: 
;   (make-ufo Posn Vel)
;    interpretation (make-ufo p v) is at location
;    p moving at velocity v

;Posn, Vel -> Posn
; adds Vel to Posn
(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))

; UFO -> UFO
; determines where u moves in one clock tick;
; leaves the velocity as it is
(check-expect (ufo-move-1 (make-ufo (make-posn 5 6) (make-vel 3 6))) (make-ufo (make-posn 8 12) (make-vel 3 6)))

(define (ufo-move-1 ufo1)
  (make-ufo (posn+ (ufo-loc ufo1) (ufo-vel ufo1)) (ufo-vel ufo1)))

(define ufo-test (make-ufo (make-posn 5 4) (make-vel 2 1)));testing examples
 (define v1 (make-vel 8 -3))
 (define v2 (make-vel -5 -3))
 (define p1 (make-posn 22 80))
 (define p2 (make-posn 30 77))
 
 (define u1 (make-ufo p1 v1))
 (define u2 (make-ufo p1 v2))
 (define u3 (make-ufo p2 v1))
 (define u4 (make-ufo p2 v2))