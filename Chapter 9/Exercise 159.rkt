;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 159|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Natural Number, Img -> Img
;creates a vertical arrangement of n copies of img
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))


(define row-height 10)
(define col-width 10)
(define cell (rectangle col-width row-height "outline" "black"))

(define dot-width (/ row-height 5))
(define dot-color "red")
(define dot (circle dot-width "solid" dot-color))

(define cols 8)
(define rows 18)

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List of posns)
; A List-of-posns is one of
; -'()
; -(cons (Posn List-of-posns))
; interpretation (make-pair n lob) means n baloons must
; yet be thrown and added to lob


;Natural Number, Natural Number -> Img
;creates an N x N grid
(define (grid r c)
  (row r (col c cell)))

(define (table-maker r c)
  (place-image (grid r c)
               (/ (* r row-height) 2)
               (/ (* c col-width) 2)
               (empty-scene (* r row-height) (* col-width c))))

(define (place-dot posn img)
  (place-image dot
               (* (posn-x posn) col-width)
               (* (posn-y posn) row-height)
               img))

(define (add-balloons lop)
  (cond [(empty? lop) (table-maker cols rows)]
        [(cons? lop) (place-dot (first lop) (add-balloons (rest lop)))]))

(define (gen-rand-balloon c r)
  (make-posn (random c) (random r)))

(define (render p)
  (add-balloons (pair-lob p)))

(check-random (tock (make-pair 5 '())) (make-pair 4 (cons (gen-rand-balloon cols rows) '())))
(define (tock p)
  (if (= (pair-balloon# p) 0)
      p
      (make-pair (sub1 (pair-balloon# p)) (cons (gen-rand-balloon cols rows) (pair-lob p)))))


(define (riot num#)
  (big-bang (make-pair num# '())
    [on-tick tock 1]
    [to-draw render]))

