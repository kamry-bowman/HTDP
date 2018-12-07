;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 311|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

;FT -> Number
; Accepts a FT and returns the number of persons within it
(check-expect (count-persons Gustav) 5)
(define (count-persons FT)
  (cond
    [(no-parent? FT) 0]
    [else (+ 1 (count-persons (child-father FT)) (count-persons (child-mother FT)))]))

; FT Number -> Number
; Accepts a FT and a year and returns the average age
(check-expect (average-age Gustav 1990) (/ (- (* 5 1990) 1988 1966 1965 1926 1926) 5))
(define (average-age FT year)
  (local ((define (sum-ages FT year)
            (cond [(no-parent? FT) 0]
                  [else (+ (- year (child-date FT)) (sum-ages (child-father FT) year) (sum-ages (child-mother FT) year))])))
    (/ (sum-ages FT year) (count-persons FT))))