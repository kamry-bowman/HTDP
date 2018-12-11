;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 382|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An FSM is a [List-of 1Transition]


; A 1Transition is a list of two items:
; (list FSM-State (list FSM-State Key-list))
; An FSM-State is a String that specifies a color
; An Key-list is a [List-of String] that represents key strokes
 
; data examples 
(define fsm-traffic
  '(("red" ("green" ("r"))) ("green" ("yellow" ("g"))) ("yellow" ("red" ("y")))))
 
; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay
         (text current 20 "black")
         (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (local ((define this-transition (find transitions current)))
        (if (valid-key key-event (second this-transition))
            (first this-transition)
            current)))]))

; Key-list -> Boolean
(check-expect (valid-key "m" (list "m")) #true)
(check-expect (valid-key "m" (list "n" "m")) #true)
(check-expect (valid-key "m" (list "n" "b")) #false)
(define (valid-key key l)
  (cond [(empty? l) #false]
        [(string=? (first l) key) #true]
        [else (valid-key key (rest l))]))
  
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") (list "green" (list "m")))
(check-expect (find fsm-traffic "green") (list "yellow" (list "m")))
(check-error (find fsm-traffic "black") "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

(simulate "red" fsm-traffic)

; An XMachine is a nested list of this shape:
;   `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; An XMachine is a nested list of this shape:
;   (list 'machine (list (list 'initial FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   (list 'action (list (list 'state FSM-State) (list 'next FSM-State)))

(define xm0
  '(machine ((initial "red"))
     (action ((state "red") (next "green")))
     (action ((state "green") (next "yellow")))
     (action ((state "yellow") (next "red")))))

; <machine initial="black">
;  <action state="black" next="white" />
;  <action state ="white" next="black />
; </machine>

(define xmBW
  '(machine ((initial "black"))
            (action ((state "black") (state "white")))
            (action ((state "white") (state "black")))))

; XMachine -> FSM-State
; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; XMachine -> FSM-State
; extracts and translates the transition table from xm0

(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm0)
  (find-attr (xexpr0attr xm0) 'initial))

; XMachine -> [List-of 1Transition]
; extracts the transition table from xm

(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))
