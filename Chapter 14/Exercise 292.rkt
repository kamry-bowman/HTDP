;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 292|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X X -> Boolean] [NEList-of X] -> Boolean
; determines whether l is sorted according to cmp

(check-expect ((sorted? <) '(1 2 3)) #true)
(check-expect ((sorted? <) '(2 1 3)) #false)

(define (sorted? cmp)
  (lambda (l)
    (local (; [NEList-of X] -> Boolean
            ; is l sorted according to cmp
            (define (sorted/l l)
      (cond [(empty? l) #true]
            [(empty? (rest l)) #true]
            [else (if (cmp (first l) (second l))
                      (sorted/l (rest l))
                      #false)])))
      (sorted/l l))))

; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))