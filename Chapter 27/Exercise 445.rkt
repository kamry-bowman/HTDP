;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 445|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon .01)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R, (+ R ε)]
; assume f is continuous
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
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