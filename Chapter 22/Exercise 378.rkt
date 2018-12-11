;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 378|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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