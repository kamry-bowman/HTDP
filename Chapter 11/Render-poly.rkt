;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Render-poly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))
	
	
(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))
 

; a plain background image
(define MT (empty-scene 50 50))

; Image Posn Posn -> Image
; draws a red line from Posn p to Posn q into im
#|
(check-expect (render-line MT (first triangle-p) (second triangle-p))
                (scene+line MT (posn-x (first triangle-p)) (posn-y (first triangle-p)) (posn-x (second triangle-p)) (posn-y (second triangle-p)) "red"))
|#

(define (render-line im p q)
  (scene+line im
              (posn-x p)
              (posn-y p)
              (posn-x q)
              (posn-y q)
              "red"))   


; NELoP -> Posn
; Extracts the last item from p
#|
(check-expect (last (list (make-posn 1 1))) (make-posn 1 1))
(check-expect (last (list (make-posn 1 1) (make-posn 2 2))) (make-posn 2 2))
(check-expect (last triangle-p) (third triangle-p))
(check-expect (last square-p) (fourth square-p))
|#
(define (last p)
  (cond [(empty? (rest p)) (first p)]
        [(cons? (rest p)) (last (rest p))]))


; image polygon -> image
; renders the given polygon p into img
#|
(check-expect (render-poly MT triangle-p) (scene+line
                                           (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red")
                                           30 20 20 10 "red"))
(check-expect (render-poly MT square-p) (scene+line
                                         (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red")
                                         10 20 10 10 "red"))
|#


(define (render-poly img p)
  (render-line (connect-dots img p)
               (first p)
               (last p)))


;;;;More general approach

; An Nelop is one of
; -(cons Posn '())
; -(cons Posn NELOP)

; Image NELop -> Image
; connects the dots in p by rendering lines in img
#|
(check-expect (connect-dots MT triangle-p) (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red"))

(check-expect (connect-dots MT square-p) (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red"))
|#
                               
(define (connect-dots img p)
  (cond [(empty? (rest p)) img]
        [(cons? (rest p)) (render-line
                           (connect-dots img (rest p))
                           (first p)
                           (second p))]))

; image polygon -> image
; renders the given polygon p into img
#|
(check-expect (render-poly-v2 MT triangle-p) (scene+line
                                           (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red")
                                           30 20 20 10 "red"))

(check-expect (render-poly-v2 MT square-p) (scene+line
                                         (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red")
                                         10 20 10 10 "red"))
|#

(define (render-poly-v2 img p)
  (connect-dots img (add-last-to-front p)))

;NeLoP -> NeLop
;adds the last item in the list to the front
(define (add-last-to-front p)
  (cons (last p) p))

;List->List
;Adds an item to end of list
#|
(check-expect (add-to-end "last" '()) (cons "last" '()))
(check-expect (add-to-end "last" (cons 1 '())) (cons 1 (cons "last" '())))
(check-expect (add-to-end "last" (cons 1 (cons 2 '()))) (cons 1 (cons 2 (cons "last" '()))))
|#
(define (add-to-end el p)
  (cond [(empty? p) (cons el '())]
        [(cons? p) (cons (first p) (add-to-end el (rest p)))]))

; NeLoP -> NeLop
; Copies first list element to the end of the list
#|
(check-expect (add-first-to-end (list (make-posn 1 1))) (list (make-posn 1 1) (make-posn 1 1)))
(check-expect (add-first-to-end (list (make-posn 1 1) (make-posn 2 2))) (list (make-posn 1 1) (make-posn 2 2) (make-posn 1 1)))
(check-expect (add-first-to-end triangle-p) (cons (first triangle-p) (cons (second triangle-p) (cons (third triangle-p) (cons (first triangle-p) '())))))
|#

(define (add-first-to-end p)
  (add-to-end (first p) p))


#|
; image polygon -> image
; renders the given polygon p into img
(check-expect (render-poly-v3 MT triangle-p) (scene+line
                                           (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red")
                                           30 20 20 10 "red"))
(check-expect (render-poly-v3 MT square-p) (scene+line
                                         (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red")
                                         10 20 10 10 "red"))

|#

(define (render-poly-v3 img p)
  (connect-dots img (add-first-to-end p)))
#|

; Image NELop -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT triangle-p) (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red"))

(check-expect (connect-dots MT square-p) (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red"))
                               
; Image NELop -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT triangle-p) (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red"))

(check-expect (connect-dots MT square-p) (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red"))

|#
                               
; Image Polygon Posn -> Img
; Consumes an image with poly drawn on top, and adds a posn to the last posn in the poly
(define (connect-dots-v2 img pos1 pos2)
  (render-line
   img
   pos1
   pos2))

(define (render-polygon-plus-last img poly+last)
  (cond
    [(empty? (rest poly+last)) img]
    [(cons? (rest poly+last)) (connect-dots-v2 (render-polygon-plus-last img (rest poly+last)) (first poly+last) (second poly+last))]))

; Image Polygon -> Image
; Draws the Polyon on the image
(check-expect (render-polygon-v4 MT triangle-p) (scene+line
                                           (scene+line
                                            (scene+line MT 20 10 20 20 "red")
                                            20 20 30 20 "red")
                                           30 20 20 10 "red"))
(check-expect (render-polygon-v4 MT square-p) (scene+line
                                         (scene+line
                                          (scene+line
                                           (scene+line MT 10 10 20 10 "red")
                                           20 10 20 20 "red")
                                          20 20 10 20 "red")
                                         10 20 10 10 "red"))

(define (render-polygon-v4 img poly)
  (cond
    [(empty? (rest poly)) img]
    [(cons? poly) (render-polygon-plus-last img (add-first-to-end poly))]))
                                                     
