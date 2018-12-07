;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 141|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;List-of-strings -> String
;Cat receives a list-of-strings and combines them into a string
(check-expect (cat (cons "a" (cons "b" '()))) "ab")
(check-expect (cat (cons "ab" (cons "cd" (cons "ef" '())))) "abcdef")

(define (cat ls)
  (cond [(empty? ls) ""]
        [(cons? ls) (string-append (first ls)
                                   (cat (rest ls)))]))