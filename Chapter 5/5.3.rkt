;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;computes the distance of ap to the origin
(check-expect (distance-to-0 (make-posn 0 5)) 5)
(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)

(define (distance-to-0 ap)
  (sqrt
   (+ (sqr (posn-x ap))
      (sqr (posn-y ap)))))


(define-struct ball [location velocity])
(define-struct vel [deltax deltay])

; A Centry is a structure:
; (make-centry String Phone Phone Phone)
; interpretation: (make-centry name home office cell) creates
;; an entry with a String name then three Phone structures for
;; different locations
(define-struct centry [name home office cell])

; A Phone is a structure:
; (make-phone Number Number)
; interpretation: (make-phone area number) creates a Phone
;; structure with a 3-digit Number as area-code, and a
;; 7 digit number as phone number
(define-struct phone [area number])


; A Phone# is a structure:
; (make-phone# Number Number Number)
; interpretation: (make-phone# (area switch num) creates a
;; phone# structure with any positive 3 digit integer as area,
;; any positive 3 digit integer as switch, and any positive 4
;; digit number as num
(define-struct phone# [area switch num])

; posn-up-x is a function
;; (posn-up-x Posn Number)
;; interpretation: (posn-up-x p n) consumes a p Posn structure and a n Number, replaces
;; the posn-x of p with n, and keeps posn-y constant
(define (posn-up-x p n)
  (make-posn n (posn-y p)))


; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-down")
  (make-posn 29 31))
(check-expect
  (reset-dot (make-posn 10 20) 29 31 "button-up")
  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond [(string=? me "button-down") (make-posn x y)]
        [else p]))
