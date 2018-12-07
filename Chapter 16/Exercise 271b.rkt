;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Exercise 271b|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;[x][list-of x][x->boolean] -> boolean
;[list-of String] [String->Boolean] -> Boolean
;Determiens whether all strings in list start with a
(check-expect (all-a-words '("barf" "ardvark" "antipathy")) #false)
(check-expect (all-a-words '("ass" "ary Poter" "airy")) #true)
(define (all-a-words los)
  (local (;String -> Boolean
          ;Determines whether String starts with "a"
          (define (a-word? string)
            (starts-with? "a" string)))
    (andmap a-word? los)))


;String String -> Boolean
;Determines whether String starts with String\
(check-expect (starts-with? "a" "bar") #false)
(check-expect (starts-with? "b" "bar") #true)
(define (starts-with? letter string)
  (string=? (substring string 0 1)
            letter))

