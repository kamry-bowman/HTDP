;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3D Exercise|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An R3 is a structure:
;   (make-r3 Number Number Number)
;   interpretation: (make-r3 x y z) accepts
;   3 numbers that resprectively represent a
;   points distance from the origin on the x,
;   y, and z planes
;   given: 3 5 2, expect: (make-r3 3 5 2)
(define-struct r3 [x y z])

 
(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))


; R3 -> Number
; R3-to-origin accepts an R3 structure, and
; calculates the distance to the R3 point from
; the origin
;;; Given:  5 , Expect: 8.77..
;(check-within (r3-to-origin (make-r3 5 4 6)) (sqrt (+ (sqr 5) (sqr 4) (sqr 6))) .0001)
;;; Given: 1, 20, -5, Expect: 20.64..
;(check-within (r3-to-origin (make-r3 1 20 -5)) (sqrt (+ (sqr 1) (sqr 20) (sqr -5))) .0001)
(define (r3-to-origin p)
  (sqrt (+ (sqr (r3-x p)) (sqr (r3-y p)) (sqr (r3-z p)))))

; A Dtime is a structure
; make-dtime (Number Number Number)
; interpretation: (make-dtime hour minute second) creates a
; dtime structure with the first number representing hour (integers between 0-23), second number minutes (integers between 0 and 59), and the third second (integers between 0 and 59), with these numbers representing time passed in a day since midnight

(define-struct dtime [hour minute second])


; Dtime -> Number
; Consumes a Dtime structure, and returns the number
; of seconds that have passed since midnight
; Given: (make-dtime 1 1 0) Returns: 3,660
;;(check-expect (time->seconds (make-dtime 1 1 0)) 3660)
; Given: (make-dtime 0 0 50) Returns: 50
;;(check-expect (time->seconds (make-dtime 0 0 50)) 50)
(define (time->seconds t)
  (+ (* 3600 (dtime-hour t)) (* 60 (dtime-minute t)) (dtime-second t)))

; A 3word is a structure
; make-3word(1String 1String 1String)
; interpretation: creates a 3letWord structure with each
; field representing a single-letter string in order for
; a three letter word. If 1String is #false, it represents a space
(define-struct 3word [s1 s2 s3])

;3word, 3word, Number -> 1String
;Consumes two 3words, and a number, and either returns the 1String
;if the number's field (1 means 1st field, 2 means 2nd field, 3 means 3rd field
; in both 3words are the same, or returns #false
(define (compare-letter word1 word2 num)
  (cond [(equal? num 1) (if
                         (equal? (3word-s1 word1) (3word-s1 word2))
                         (3word-s1 word1)
                         #false)]
        [(equal? num 2) (if
                         (equal? (3word-s2 word1) (3word-s2 word2))
                         (3word-s2 word1)
                         #false)]
        [(equal? num 3) (if
                         (equal? (3word-s3 word1) (3word-s3 word2))
                         (3word-s3 word1)
                         #false)]))
        

                
;3word -> Boolean
;compare-word consumes a 3word structure, and checks if the words
;are the same. If they are the same, it returns the original 3word.
;If they are different, returns the original 1String for fields that
;matched, and #false for fields that did not
;Given: 3word version of dog and dog, returns 3word dog
(check-expect (compare-word (make-3word "d" "o" "g") (make-3word "d" "o" "g")) (make-3word "d" "o" "g"))
;Given: 3word version of god and dog, returns 3word [#false o #false]
(check-expect (compare-word (make-3word "g" "o" "d") (make-3word "d" "o" "g")) (make-3word #false "o" #false))
;;
(define (compare-word word1 word2)
  (make-3word (compare-letter word1 word2 1)
              (compare-letter word1 word2 2)
              (compare-letter word1 word2 3)))
        