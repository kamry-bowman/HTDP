;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 138|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
; ex: (cons 5 (cons 3 (cons '())))

;List-of-amounts -> Number
;Takes a sum of each amount in list
(check-expect (sum '()) 0)
(check-expect (sum (cons 4 '())) 4)
(check-expect (sum (cons 3 (cons 4 '()))) 7)

(define (sum l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (first l)
                  (sum (rest l)))]))

;List-of-numbers is one of:
;- '()
;- (cons Number List-of-numbers)

;List-of-numbers -> Boolean
;;Takes a list, returns true if list is only positive numbers
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 4 '())) #true)
(check-expect (pos? (cons 3 (cons 4 '()))) #true)
(check-expect (pos? (cons 3 (cons -2 '()))) #false)

(define (pos? l)
  (cond [(empty? l) #true]
        [(cons? l) (and (>= (first l) 0)
                        (pos? (rest l)))]))

;List-of-numbers -> Number
;;Takes a list, returns 
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 4 '())) 4)
(check-expect (checked-sum (cons 3 (cons 4 '()))) 7)
(check-error (checked-sum (cons 3 (cons -2 '()))) "Error")
(define (checked-sum l)
  (cond [(pos? l) (sum l)]
        [else (error "Error")]))