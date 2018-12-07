;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 142|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;loi
;;either:
;; -'()
;; -(cons Image loi)

;ill-sized helper function
;checks image size to see if its within NxN
(define (image-ill-sized? img n)
  (if (and (= (image-width img) n)
           (= (image-height img) n))
      #false
      #true))
      

;;LOI, Number -> Image or #false
;;returns the first image that is non an NxN square, otherwise #false
(check-expect (ill-sized? '() 10) #false)
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black") '()) 10) #false)
(check-expect (ill-sized? (cons (rectangle 10 15 "solid" "black") '()) 10) (rectangle 10 15 "solid" "black"))
(check-expect (ill-sized? (cons (rectangle 10 15 "solid" "black") '()) 10) (rectangle 10 15 "solid" "black"))
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black") (cons (rectangle 10 15 "solid" "black") '())) 10) (rectangle 10 15 "solid" "black"))
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black") (cons (rectangle 10 10 "solid" "black") '())) 10) #false)
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "black") (cons (rectangle 10 12 "solid" "black") (cons (rectangle 10 20 "solid" "black") '()))) 10) (rectangle 10 12 "solid" "black"))

              
(define (ill-sized? loi n)
  (cond [(empty? loi) #false]
        [(cons? loi) (if (image-ill-sized? (first loi) n)
                         (first loi)
                         (ill-sized? (rest loi) n))]))