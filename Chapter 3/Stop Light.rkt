;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Stop Light|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;constants
(define LIGHT-RADIUS 10)
(define WORLD-WIDTH (* 2 LIGHT-RADIUS))
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-WIDTH))


;WS -> TrafficLight
;Tracks elapsed time
(define (tock ws)
  (traffic-light-next ws))

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

(define (light-image ws)
  (circle LIGHT-RADIUS "solid" ws))
  
;WS -> image
(define (render ws)
  (place-image (light-image ws) LIGHT-RADIUS LIGHT-RADIUS BACKGROUND))

;main big-bang function
(define (main init-light)
  (big-bang "red"
    [on-tick tock]
    [to-draw render]))