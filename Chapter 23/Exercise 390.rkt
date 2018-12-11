;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 390|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; N is one of: 
; – 0
; – (add1 N)


; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l; 
; signals an error if there is no such symbol
(define (list-pick l n)
    (cond
    [(empty? l) (error "list too short")]
    [(= n 0) (first l)]
    [else (list-pick (rest l) (sub1 n))]))

(check-expect (list-pick '(a b c) 2) 'c)
(check-error (list-pick '() 0) "list too short")


(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error (list-pick '() 3) "list too short")

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; TOS [List-of Direction] => TOS
(define (tree-pick tos lod)
  (cond
    [(empty? lod) tos]
    [(symbol? tos) (error "tree lacks depth")]
    [else (if (symbol=? (first lod) 'left)
              (tree-pick (branch-left tos) (rest lod))
              (tree-pick (branch-right tos) (rest lod)))]))

(define branch0 (make-branch 'a 'b))
(define branch1 (make-branch 'c 'd))
(define branch2 (make-branch branch0 branch1))
(define branch3 (make-branch branch2 'e))

(check-error (tree-pick 'a '(left)) "tree lacks depth")
(check-expect (tree-pick 'a '()) 'a)
(check-expect (tree-pick branch3 '(right)) 'e)
(check-expect (tree-pick branch3 '(left right left)) 'c)
(check-expect (tree-pick branch3 '(left right)) branch1)
