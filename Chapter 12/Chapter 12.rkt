;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Chapter 12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))


; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines "reduced-dict.txt"))


(define (file-reduce list num)
  (cond [(empty? list) ""]
        [(cons? list) (if (= (modulo num 15) 0)
                          (string-append (first list) "\n" (file-reduce (rest list) (add1 num)))
                         (file-reduce (rest list) (add1 num)))]))

(define (write-third-file name list) (write-file name (file-reduce list 0)))

; 1String Dictionary -> Number
; Returns a Number of the number of words in Dictionary that start with 1String
;(check-expect (starts-with# "a" '()) 0)
;(check-expect (starts-with# "a" (list "attica")) 1)
;(check-expect (starts-with# "a" (list "bat" "attica")) 1)
;(check-expect (starts-with# "a" (list "ardvark" "bat" "attica")) 2)

(define (starts-with# letter dict)
  (cond [(empty? dict) 0]
        [(cons? dict) (if (starts-with? letter (first dict))
                          (add1 (starts-with# letter (rest dict)))
                          (starts-with# letter (rest dict)))]))

; Letter, Dictionary -> #Boolean
; Returns true if there are no strings that start with letter in dictionary, else false
;(check-expect (no-starting-letter "a" '()) #true)
;(check-expect (no-starting-letter "a" (list "barf")) #true)
;(check-expect (no-starting-letter "a" (list "arrse")) #false)
;(check-expect (no-starting-letter "a" (list "barg" "arse" "carp")) #false)

(define (no-starting-letter letter dict)
  (cond [(empty? dict) #true]
        [(empty? (rest dict)) (not (starts-with? letter (first dict)))]
        [(cons? dict) (if (starts-with? letter (first dict))
                          #false
                          (no-starting-letter letter (rest dict)))]))

; 1String String -> Boolean
; Checks whether the String starts with 1String, returns #true if so
;(check-expect (starts-with? "a" "ashtray") #true)
;(check-expect (starts-with? "a" "zoo") #false)

(define (starts-with? letter word)
  (string=? letter (string-ith word 0)))

; A Letter-Count is a Structure
; (make-LC 1String Number) represents a "letter" and the number of occurences
; of the 1String in a dictionary
(define-struct LC [letter count])
; A List-of-Letter-Counts is Either
; -'()
; -(cons LC List-of-Letter-Counts)


; Dictionary -> List-of-Letter-Counts
; Consumes a dictionary, and returns a list of letter counts for each letter
(define (count-by-letter dict)
  (count-dict-by-list dict LETTERS))

; Dictionary Letters -> List-of-Letter-Counts
; Consumes a dictionary and a list of letters, returns a list of Letter-of-counts for each letter in the list
;(check-expect (count-dict-by-list (list "ardvark") (list "a")) (list (make-LC "a" 1)))
;(check-expect (count-dict-by-list (list "bardvark") (list "a")) (list (make-LC "a" 0)))
;(check-expect (count-dict-by-list (list "bardvark" "bastard" "carpark") (list "a" "b" "c")) (list (make-LC "a" 0) (make-LC "b" 2) (make-LC "c" 1)))

(define (count-dict-by-list dict l)
  (cond [(empty? l) '()]
        [(cons? l) (cons (make-LC (first l) (starts-with# (first l) dict))
                      (count-dict-by-list dict (rest l)))]))

; List-of-Letter-Counts -> List-of-Letter-Counts
; Consumes a list of letter counts, and returns a list sorted by highest to lowest letter count
;(check-expect (sort-most-frequent-letter '()) '())
;(check-expect (sort-most-frequent-letter (list (make-LC "a" 2))) (list (make-LC "a" 2)))
;(check-expect (sort-most-frequent-letter (list (make-LC "a" 1) (make-LC "b" 2))) (list (make-LC "b" 2) (make-LC "a" 1)))
;(check-expect (sort-most-frequent-letter (list (make-LC "a" 2) (make-LC "b" 1))) (list (make-LC "a" 2) (make-LC "b" 1)))
;(check-expect (sort-most-frequent-letter (list (make-LC "a" 4) (make-LC "b" 6) (make-LC "c" 1) (make-LC "d" 5))) (list (make-LC "b" 6) (make-LC "d" 5) (make-LC "a" 4) (make-LC "c" 1)))

(define (sort-most-frequent-letter lolc)
  (cond [(empty? lolc) '()]
        [(cons? lolc) (insert-LC (first lolc) (sort-most-frequent-letter (rest lolc)))]))

; List-of-letter-counts, Letter-Count -> List-of-letter counts
; Consumes a list of letter-counts, and places a letter count in the correct place relative to the list of letter-counts
;(check-expect (insert-LC (make-LC "a" 5) '()) (list (make-LC "a" 5)))
;(check-expect (insert-LC (make-LC "b" 1) (list (make-LC "a" 5))) (list (make-LC "a" 5) (make-LC "b" 1)))
;(check-expect (insert-LC (make-LC "b" 4) (list (make-LC "a" 5) (make-LC "c" 3))) (list (make-LC "a" 5) (make-LC "b" 4) (make-LC "c" 3)))

(define (insert-LC lc lolc)
  (cond [(empty? lolc) (list lc)]
        [(cons? lolc) (if (>= (LC-count lc) (LC-count (first lolc)))
                          (cons lc (insert-LC (first lolc) (rest lolc)))
                          (cons (first lolc) (insert-LC lc (rest lolc))))]))

;List of Letter-counts -> Letter-count
;Consumes a list of letter-count, and returns the highest letter-count
(define (most-frequent lolc)
  (first (sort-most-frequent-letter (count-by-letter lolc))))

; Dictionary -> List-of-dictionaries
; Consumses a dictionary, and returns a list of dictionaries, each reflecting
; The strings from the first dictionary that share the same first letter

(define (words-by-first-letter dict)
  (words-by-first-letter-list dict LETTERS))

; Dictionary, list of 1Strings -> List of Dictionaries
;(check-expect (words-by-first-letter-list (list "alpha" "beta") (list "a" "b")) (list (list "alpha") (list "beta")))
(define (words-by-first-letter-list dict lol)
  (cond [(empty? lol) '()]
        [(cons? lol) (if (no-starting-letter (first lol) dict)
                         (words-by-first-letter-list dict (rest lol))
                         (cons (dict-for-letter-only (first lol) dict) (words-by-first-letter-list (dict-after-letter (first lol) dict) (rest lol))))]))

; Letter, Dictionary -> Dictionary
; Produces a dictionary with only strings starting with the given letter
;(check-expect (dict-for-letter-only "a" (list "apple" "amway" "backalley")) (list "apple" "amway"))
(define (dict-for-letter-only letter dict)
  (cond [(empty? dict) '()]
        [(cons? dict) (if (starts-with? letter (first dict))
                          (cons (first dict) (dict-for-letter-only letter (rest dict)))
                          (dict-for-letter-only letter (rest dict)))]))

; Letter, Dictionary -> Dictionary
; Produces a dictionary without any words starting with the given letter
;(check-expect (dict-after-letter "a" (list "apple" "amway" "backalley")) (list "backalley"))
(define (dict-after-letter letter dict)
  (cond [(empty? dict) '()]
        [(cons? dict) (if (starts-with? letter (first dict))
                          (dict-after-letter letter (rest dict))
                          (cons (first dict) (dict-after-letter letter (rest dict))))]))

;List of Letter-counts -> Letter-count
;Consumes a list of letter-count, and returns the highest letter-count
(define (most-frequent.v2 dict)
  (dictionary-first-letter (longest-list (words-by-first-letter dict))))

;List of lists -> List
;Consumes the longest list, and returns the longest list
(check-expect (longest-list '()) '())
(check-expect (longest-list (list (list 1))) (list 1))
(check-expect (longest-list (list (list 2 2) (list 1))) (list 2 2))
(check-expect (longest-list (list (list 1) (list 2 2))) (list 2 2))
(check-expect (longest-list (list (list 1) (list 3 3 3) (list 2 2))) (list 3 3 3))
(define (longest-list lol)
  (cond [(empty? lol) '()]
        [(empty? (rest lol)) (first lol)]
        [(cons? (rest lol)) (if (> (length (first lol)) (length (first (rest lol))))
                                (longest-list (cons (first lol) (rest (rest lol))))
                                (longest-list (rest lol)))]))

; Dictionary -> 1String
; Returns the first string of the first string in a dictionary
(check-expect (dictionary-first-letter (list "ark" "covenant")) "a")
(define (dictionary-first-letter dict)
  (string-ith (first dict) 0))