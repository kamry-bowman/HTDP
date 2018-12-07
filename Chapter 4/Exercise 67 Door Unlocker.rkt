;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 67 Door Unlocker|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Constants
; A DoorState is one of:
(define LOCKED "locked")  ; – LOCKED
(define CLOSED "closed")  ; – CLOSED
(define OPEN "open")      ; – OPEN

;DoorState -> DoorState
;Updates door state with each tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

(define (door-closer ds)
  (cond
    [(string=? ds LOCKED) LOCKED]
    [(string=? ds CLOSED) CLOSED]
    [(string=? ds OPEN) CLOSED]))

;key-event, DoorState -> DoorState
;Analyzes key event and doorstate, returns doorstate
(check-expect (door-action LOCKED "u") CLOSED)
(check-expect (door-action CLOSED "l") LOCKED)
(check-expect (door-action CLOSED " ") OPEN)
(check-expect (door-action CLOSED "a") CLOSED)
(check-expect (door-action LOCKED "l") LOCKED)
(check-expect (door-action OPEN "l") OPEN)

(define (door-action ds ke)
  (cond
    [(and (string=? ds LOCKED) (string=? ke "u")) CLOSED]
    [(and (string=? ds CLOSED) (string=? ke "l")) LOCKED]
    [(and (string=? ds CLOSED) (string=? ke " ")) OPEN]
    [else ds]))

;DoorState -> Image
;Draws an image based on Doorstate
(check-expect (render CLOSED) (text CLOSED 40 "red"))

(define (render ds)
  (text ds 40 "red"))

(define (main ds)
  (big-bang ds
  [on-tick door-closer 3]
  [on-key door-action]
  [to-draw render]))