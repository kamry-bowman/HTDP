;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 452|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define NEWLINE "\n")
; A File is one of:
; - '()
; - (cons "\n" File)
; - (cons 1String File)
; interpretation represents the content of a file
; "\n" is the newline character

; A Line is a [List-of 1String]

; File -> [List-of Line]
; converts a file into a list of lines
(check-expect (file->list-of-lines
               (list "a" "b" "c" "\n"
                     "d" "e" "\n"
                     "f" "g" "h" "\n"))
              '(("a" "b" "c")
                ("d" "e")
                ("f" "g" "h")))

; File -> [List-of Line]
; converts a file into a list of lines
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))

; File -> Line
; consumes a File, and returns the first line, either '() if
; File is '(), or a list of all 1Strings up until the first "\n".
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))

; File -> Line
; Consumes a File, and returns '() if list is empty, otherwise
; returns the list of 1Strings and/or "\n" following the first occurence
; of "\n"
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))

; A Token is one of:
; - 1String
; - String consisting of lower-case letters

; Line -> [List-of Token]
; Consuems a Line and converts it into a List-of Tokens
(check-expect (tokenize '("h" "i" " " "b" "a" "r" "t")) '("hi" "bart"))
(check-expect (tokenize '("h" "i" " " " " "b" "a" "r" "t")) '("hi" "bart"))
(define (tokenize aline)
  (cond
    [(empty? aline) '()]
    [else (cons (first-token aline) (tokenize (remove-first-token aline)))]))

; Line -> Token
(define (first-token aline)
  (cond
    [(empty? aline) ""]
    [(string-whitespace? (first aline)) ""]
    [else (string-append (first aline) (first-token (rest aline)))]))

; Line -> Line
(define (remove-first-token aline)
  (cond
    [(empty? aline) '()]
    [(string-whitespace? (first aline))
     (if (and (cons? (rest aline))
              (string-whitespace? (second aline)))
         (remove-first-token (rest aline))
         (rest aline))]
    [else (remove-first-token (rest aline))]))



    




