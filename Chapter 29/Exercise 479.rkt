;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 479|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column

(define q00 (make-posn 0 0))
(define q07 (make-posn 0 7))
(define q70 (make-posn 7 0))
(define q44 (make-posn 4 4))
(define q43 (make-posn 4 3))
(define q55 (make-posn 5 5))
(define q33 (make-posn 3 3))
(define q22 (make-posn 2 2))
(define q62 (make-posn 6 2))
(define q63 (make-posn 6 3))
(define q31 (make-posn 3 1))
(define q57 (make-posn 5 7))

(check-expect (threatening? q00 q07) #true)
(check-expect (threatening? q00 q70) #true)
(check-expect (threatening? q00 q43) #false)
(check-expect (threatening? q44 q55) #true)
(check-expect (threatening? q44 q33) #true)
(check-expect (threatening? q44 q22) #true)
(check-expect (threatening? q44 q62) #true)
(check-expect (threatening? q44 q63) #false)
(check-expect (threatening? q31 q57) #false)

(define (threatening? qp1 qp2)
  (local ((define xdiff (- (posn-x qp1) (posn-x qp2)))
          (define ydiff (- (posn-y qp1) (posn-y qp2))))
  (cond
    [(= xdiff 0) #true]
    [(= ydiff 0) #true]
    [(= xdiff ydiff) #true]
    [(= (* -1 xdiff) ydiff) #true]
    [else #false])))

(define WIDTH 20)

(define (render-board n width)
  (local ((define target (* n n))
          (define BLACK (square width 'solid 'black))
          (define WHITE (square width 'outline 'black))
          (define (helper current board)
            (cond
              [(= current target) board]
              [else (local ((define row (modulo current n))
                            (define col (quotient current n))
                            (define this_square
                              (cond [(even? row) (if (even? col) WHITE BLACK)]
                                    [else (if (even? col) BLACK WHITE)]))
                            (define x-place (* row width))
                            (define y-place (* col width)))
                      (place-image/align this_square
                                         x-place y-place
                                         "left" "top"
                                         (helper  (add1 current) board)))])))
    (helper 0 (empty-scene (* width n) (* width n)))))

; n [List-of QP] Image -> Image
(define (render-queens n loq)
  (local ((define QUEEN (overlay (circle (* WIDTH .4) 'solid 'white)
                                 (circle (* WIDTH .5) 'solid 'black)))
          (define (draw-queens loq img)
            (cond
              [(empty? loq) img]
              [else (local ((define a-queen (first loq))
                            (define row (posn-x a-queen))
                            (define col (posn-y a-queen))
                            (define x-place (* (+ row .5) WIDTH))
                            (define y-place (* (+ col .5) WIDTH)))
                      (place-image QUEEN x-place y-place
                                   (draw-queens (rest loq) img)))])))
    (draw-queens loq (render-board n WIDTH))))

(render-queens 8 `(,q00 ,q44 ,q57 ,q22 ,q63))


                            
              