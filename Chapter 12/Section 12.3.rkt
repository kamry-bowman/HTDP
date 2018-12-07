;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Section 12.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
 
; A List-of-words is one of:
; - '()
; - (cons Word List-of-Words)



;(define dictionary (list "tar" "rat" "art"))

(define dictionary
  (read-lines "dictionary.txt"))

; List-of-strings -> Boolean 
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

(check-satisfied (alternative-words "rat") all-words-from-rat?)
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; List-of-words -> List-of-strings
; turns all Words in low into Strings
(define (words->strings low)
  (cond [(empty? low) '()]
        [(cons? low) (cons (word->string (first low)) (words->strings (rest low)))]))

;List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
;(check-expect (in-dictionary (list "barf" "dasfadf")) (list "barf"))
(define (in-dictionary los)
  (cond [(empty? los) '()]
        [(cons? los) (if (member? (first los) dictionary)
                         (cons (first los) (in-dictionary (rest los)))
                         (in-dictionary (rest los)))]))


; Word -> List-of-words
; finds all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [(cons? w) (insert-everywhere/in-all-words (first w) (arrangements (rest w)))]))

; 1String, List-of-Words -> List-of-words
; Inserts letter at each position from beginning to end, for all words in list-of-words, and adds each permutation to a list-of-words, which returns
;(check-expect (insert-everywhere/in-all-words "a" (list '())) (list (list "a")))
;(check-expect (insert-everywhere/in-all-words "a" (list (list "1"))) (list (list "1" "a") (list "a" "1")))
;(check-expect (insert-everywhere/in-all-words "a" (list (list "1") (list "2"))) (list (list "1" "a") (list "a" "1") (list "2" "a") (list "a" "2")))
;(check-expect (insert-everywhere/in-all-words "a" (list (list "1" "1"))) (list (list "1" "1" "a") (list "1" "a" "1") (list "a" "1" "1")))
(define (insert-everywhere/in-all-words letter low)
  (cond [(empty? low) '()]
        [(cons? low) (append (insert-everywhere/in-word letter (first low))
                          (insert-everywhere/in-all-words letter (rest low )))]))

;1String, Word -> List of Words
; Inserts letter in each possible space in word from beginning to end, returning a list of words produced by this
;(check-expect (insert-everywhere/in-word "a" '()) (list (list "a")))
;(check-expect (insert-everywhere/in-word "a" (list "1")) (list (list "1" "a")  (list "a" "1")))
;(check-expect (insert-everywhere/in-word "a" (list "2" "2")) (list (list "2" "2" "a") (list "2" "a" "2") (list "a" "2" "2") ))
(define (insert-everywhere/in-word letter word)
  (cond [(empty? word) (list (list letter))]
        [(cons? word) (process-word letter (reverse word) '())]))

; Word, 1String, Word, List of Words -> List of words
; Creates a new word by combining front-list, letter, and back-list, and adding that to a list, then calling itself again after shifting the last front-list (in reversed order) onto the back-list. The result is a word-list with the letter placed at each position from start to beginning
;(check-expect (process-word "a" '() '()) (list (list "a")))
;(check-expect (process-word "a" (list "2" "1") '()) (list (list "1" "2" "a") (list "1" "a" "2") (list "a" "1" "2")))
(define (process-word letter front-list back-list)
  (cond [(empty? front-list) (list (cons letter back-list))]
        [(cons? front-list) (cons (append (reverse front-list) (cons letter back-list)) (process-word letter (rest front-list) (cons (first front-list) back-list)))]))

; String -> Word
; converts s to the chosen word representation
(define (string->word s)
  (explode s))

; Word->String
; converts w to a string
(define (word->string w)
  (implode w))

(alternative-words "rat")