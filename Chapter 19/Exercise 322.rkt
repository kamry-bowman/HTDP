;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 322|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define BTA
  (make-node
   15
   'd
   NONE
   (make-node
    24 'i NONE NONE)))

(define BTB
  (make-node
   15
   'd
   (make-node
    87 'h NONE NONE)
   NONE))


; BT -> Boolean
; Determines whether a given number occurs in a BT
(check-expect (contains-bt? BTB 87) #true)
(check-expect (contains-bt? BTB 15) #true)
(check-expect (contains-bt? BTA 87) #false)
(define (contains-bt? BT num)
  (match BT
    [(no-info) #false]
    [(node ssn name L R) (or (= ssn num)
                             (contains-bt? L num)
                             (contains-bt? R num))]))