;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 492|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A SimpleGraph is a [List-of Connection]
; A Connection is a list of two items:
;   (list Node Node)
; A Node is a Symbol.

(define a-sg
  '((A B)
    (B C)
    (C E)
    (D E)
    (E B)
    (F F)))

; Node Node SimpleGraph -> Boolean
; is there a path from origin to destination
; in the simple graph sg

(check-expect (path-exists? 'A 'E a-sg) #true)
(check-expect (path-exists? 'A 'F a-sg) #false)

(define (path-exists? origin destination sg)
  (local ((define (path-exists?/a origin destination sg seen)
             (cond
               [(symbol=? origin destination) #true]
               [(member? origin seen) #f]
               [else (path-exists?/a (neighbor origin sg)
                                     destination
                                     sg
                                     (cons origin seen))])))
    (path-exists?/a origin destination sg '())))

; Node Node SimpleGraph [List-of Node] -> Boolean
; is there a path from origin to destination
; assume there are no paths for the nodes in seen


; Node SimpleGraph -> Node
; determine the node taht is connected to a-node in sg
(check-expect (neighbor 'A a-sg) 'B)
(check-error (neighbor 'G a-sg) "neighbor: not a node")
(define (neighbor a-node sg)
  (cond
    [(empty? sg) (error "neighbor: not a node")]
    [else (if (symbol=? (first (first sg)) a-node)
              (second (first sg))
              (neighbor a-node (rest sg)))]))