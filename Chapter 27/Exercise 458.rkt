;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 458|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define epsilon .01)
(define epsilon-u .001)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between
; a and b
; assume (< a b) holds
(define (integrate f left right)
  (integrate-adaptive f left right))

(define (constant x) 20)
(check-within (integrate constant 12 22) 200 epsilon)

(define (linear x) (* 2 x))
(check-within (integrate linear 0 10) 100 epsilon)

(define (square x) (* 3 (sqr x)))
(check-within (integrate square 0 10)
              (- (expt 10 3) (expt 0 3))
              epsilon)

(define (integrate-kepler f l r)
  (local ((define f@r (f r))
          (define f@l (f l)))
    (+
     (* (- r l) f@r)
     (* .5 (- r l) (- f@l f@r)))))

(define (integrate-rect f left right R)
  (local ((define W (/ (- right left) R))
          (define S (/ W 2))
          (define (add-rect f i total)
            (cond
              [(= i 0) (+ total (* W (f (+ left S))))] 
              [else (add-rect f (sub1 i) (+ total (* W (f (+ left (* W i) S)))))])))
    (add-rect f (- R 1) 0)))

(define (integrate-dc f left right)
  (local ((define width (- right left))
          (define half-w (/ width 2)))
    (cond
      [(<= width epsilon) (integrate-kepler f left right)]
      [else (+ (integrate-dc f left (+ left half-w)) (integrate-dc f (+ left half-w) right))])))

(define (integrate-adaptive f left right)
  (local ((define (integrate-helper left right full-t)
            (local ((define width (- right left))
                    (define half-w (/ width 2))
                    (define eps-rect (* epsilon-u width)) 
                    (define left-trapezoid (integrate-kepler f left (+ left half-w)))
                    (define right-trapezoid (integrate-kepler f (+ left half-w) right)))
              (cond
                [(<= (abs (- full-t left-trapezoid right-trapezoid)) eps-rect) full-t]
                [else
                 (+
                  (integrate-helper
                   left
                   (+ left half-w)
                   left-trapezoid)
                  (integrate-helper
                   (+ left half-w)
                   right
                   right-trapezoid))]))))
    (integrate-helper left right (integrate-kepler f left right))))
