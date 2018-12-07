;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 318|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr -> Boolean
; Determines whether S-expr is an Atom
(check-expect (atom? '(3 4)) #false)
(check-expect (atom? 'k) #true)
(define (atom? sexp)
  (or (number? sexp)
      (string? sexp)
      (symbol? sexp)))

; S-expr -> N 
; Consumes an S-expression and determines its depth, where an S-expression that is an Atom has a depth of 1, and the depth of a SL is the maximum depth of its items plus 1.
(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello))) 4)
(define (depth sexp)
  (local ((define (depth-sexp sexp)
           (cond [(atom? sexp) 1]
                 [else (depth-sl sexp)]))
          (define (depth-sl sl)
            (cond [(empty? sl) 1]
                  [else (max
                         (+ 1 (depth-sexp (first sl)))
                         (depth-sl (rest sl)))])))
    (depth-sexp sexp)))
