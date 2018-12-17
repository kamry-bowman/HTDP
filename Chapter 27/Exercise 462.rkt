;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 462|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r[1] ... r[n]), (length r[i]) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a[1] ... a[n] b) is an Equation, 
; a[1], ..., a[n] are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2 3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
; Determines if Solution is a solution to SOE
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(3 2 1)) #false)
(define (check-solution SOE Solution)
  (cond
    [(empty? SOE) #true]
    [else (local ((define row (first SOE))
                  (define equation (lhs row))
                  (define expectation (rhs row)))
            (if (= expectation (plug-in equation Solution))
                (check-solution (rest SOE) Solution)
                #false))]))

; Equation Solution -> Number
; Consumes an Equation and a Solution, yielding value of
; the equation expression when the solution is plugged in
(check-expect (plug-in '(2 2 3) S) 10)
(check-expect (plug-in '(2 5 12) S) 31)
(check-expect (plug-in '(4 1 -2) S) 1)
(define (plug-in e s)
  (local ((define (helper e s total)
            (cond
              [(empty? e) total]
              [else (helper
                     (rest e)
                     (rest s)
                     (+ total (* (first s) (first e))))])))
    (helper e s 0)))



