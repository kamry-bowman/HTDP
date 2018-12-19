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




; A Board is a structure
(define-struct board [n unthreat occupied])
; - n represents the number of rows on the board
; - unthreatened is a [List-of posn] representing unthreatened positions
; - occupied is a [List-of QP]

; N -> Board 
; creates the initial n by n board
(check-expect (board0 1) (make-board 1 `(,(make-posn 0 0)) '()))
(check-expect (board0 2) (make-board 2 `(,(make-posn 1 1)
                                         ,(make-posn 0 1)
                                         ,(make-posn 1 0)
                                         ,(make-posn 0 0)) '()))
(define (board0 n)
  (local ((define target (* n n))
          (define (fill-board current lop)
            (cond
              [(= current target) lop]
              [else (local ((define x (modulo current n))
                            (define y (quotient current n))
                            (define pos (make-posn x y)))
                      (fill-board (add1 current) (cons pos lop)))]))
          (define unthreat (fill-board 0 '())))
  (make-board n unthreat '())))

; Board QP -> Board 
; places a queen at qp on a-board
(define (add-queen a-board qp)
  (local ((define (update-threat lop new)
            (filter (lambda (p) (not (threatening? new p))) lop))
          (define unthreat (board-unthreat a-board))
          (define n (board-n a-board))
          (define occupied (board-occupied a-board)))
    (make-board n (update-threat unthreat qp) (cons qp occupied))))

(define empty-board
  (make-board 3 `(,(make-posn 0 0) ,(make-posn 0 1) ,(make-posn 0 2)
                                   ,(make-posn 1 0) ,(make-posn 1 1) ,(make-posn 1 2)
                                   ,(make-posn 2 0) ,(make-posn 2 1) ,(make-posn 2 2))
              '()))
(define queen-corner
  (make-board 3 `(,(make-posn 1 2) ,(make-posn 2 1)) `(,(make-posn 0 0))))
(check-expect (add-queen empty-board (make-posn 0 0)) queen-corner)
                                  
                                

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  (board-unthreat a-board))

; N -> [[List-of QP] -> Boolean]
(define (n-queens-solution? n)
  (local ((define (process-q loq)
            (cond
              [(empty? loq) #true]
              [else (if (ormap (lambda (q2) (threatening? (first loq) q2)) (rest loq))
                        #false
                        (process-q (rest loq)))]))
          (define (pred loq)
            (if (= (length loq) n)
                (process-q loq)
                #false)))
    pred))

(define q20 (make-posn 2 0))
(define q01 (make-posn 0 1))
(define q32 (make-posn 3 2))
(define q13 (make-posn 1 3))

(define 4-good (list q20 q01 q32 q13))
(define 4-good-rev (list q13 q32 q01 q20))

(define q23 (make-posn 2 3))
(define 4-bad (list q20 q01 q32 q23))

(define check-4 (n-queens-solution? 4))
(check-satisfied 4-good check-4)
(check-satisfied 4-good-rev check-4)
(check-expect (check-4 4-bad) #false)

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(= n 0) a-board]
    [else (local ((define open-spots (find-open-spots a-board))
                  (define (process-spots los)
                    (cond
                      [(empty? los) #false]
                      [else (local ((define one-placement (add-queen a-board (first los)))
                                    (define candidate (place-queens one-placement (sub1 n))))
                              (if (boolean? candidate)
                                  (process-spots (rest los))
                                  candidate))])))
            (process-spots open-spots))]))
(check-satisfied (board-occupied (place-queens (board0 4) 4)) check-4)

(render-queens 8 (board-occupied (place-queens (board0 8) 8)))


              