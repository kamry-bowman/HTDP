;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 445|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon .01)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R, (+ R ε)]
; assume f is continuous
; assume(or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two halves
; picks according to (2)
(define (find-root f left right)
  (local ((define (helper f left right f@left f@right)
           (cond
             [(<= (- right left) epsilon) left]
             [else
              (local ((define mid (/ (+ left right) 2))
                      (define f@mid (f mid)))
                (cond
                  [(or
                    (<= f@left 0 f@mid)
                    (<= f@mid 0 f@left))
                   (helper f left mid (f left) (f mid))]
                  [(or
                    (<= f@right 0 f@mid)
                    (<= f@mid 0 f@right))
                   (helper f mid right (f mid) (f right))]))])))
    (helper f left right (f left) (f right))))
          
 

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(check-satisfied (find-root poly 3 6) (lambda (x) (or (<= (abs (- x 2)) epsilon)
                                                      (<= (abs (- x 4)) epsilon))))

(check-satisfied (find-root poly 0 6) (lambda (x) (or (<= (abs (- x 2)) epsilon)
                                                      (<= (abs (- x 4)) epsilon))))

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))

; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= -")))

(define table2 (make-table 1 a2))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

(define (find-linear t)
  (local ((define (helper i)
            (cond
              [(<= (abs (table-ref t i)) epsilon) i]
              [else (helper (add1 i))])))
    (helper 0)))

(define table3 (make-table 6 (lambda (i) (- i 4))))
(define table4 (make-table 3 (lambda (i) (- i 5))))
(check-expect (find-linear table3) 4)
(check-expect (find-linear table4) 5)

; table Number Number -> Number
; determines R such that t has a root in [R, (+ R ε)]
; assume (<= (table-ref t left) 0 (table-ref right))
; generative divides interval in half, the root is in one of the two halves
; picks according to (2)
(define (find-binary t left right)
  (local ((define (helper t left right t@left t@right)
            (local ((define mid (floor (/ (+ left right) 2)))
                    (define t@mid (table-ref t mid)))
            (cond
              [(= t@left 0) left]
              [(= t@right 0) right]
              [(<= t@left 0 t@mid) (helper t left mid t@left t@mid)]
              [(<= t@mid 0 t@right) (helper t mid right t@mid t@right)]
              [else (error "assumptions violated")]))))
    (helper t left right (table-ref t left) (table-ref t right))))

(check-expect (find-binary table3 0 30) 4)
; (check-expect (find-binary table4 0 30) 5)

 
; helper t 0 30 -4 26 .. mid = 15, t@mid = 11
; helper t 0 15 -4 11 .. mid = 7, t@mid = 3
; helper t 0 7 -4 3 .. mid = 3, t@mid = -1
; helper t 3 7 -1 3 .. mid = 5, t@mid = 1
; helper t 3 5 -1 1 .. mid = 4, t@mid = 0
; helper t 3 4 -1