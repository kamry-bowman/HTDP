;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Intermezzo quote unquote|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/web-io)

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(define one-list-web
  '(table (tr (td 1) (td "Asia: Heat of the Moment"))
          (tr (td 2) (td "U2: One"))
          (tr (td 3) (td "The White Stripes: Seven Nation Army"))))

; A Song-ranking (SR) is a list, consisting of
; A number, and a string, where the number representes the ranked order of the song, and the string represents its title

; A List of Song-rankings (LoSR) consists of either:
; - '()
; - (cons SR LoSR)

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

;  Primitive -> ... nested list ...
; creates a cell for an HTML table containing elements converted to a strng
(define (make-cell e)
  (cond [(number? e) `(td ,(number->string e))]
        [(boolean? e) `(td ,(number->string e))]
        [else e]))
        
; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers 
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

; los-> x-expression
; Consumes a list of songs, and returns a numbered html table list of these to be included in a website
(check-expect (make-rankings '("Song 1" "Song 2"))
              '(table ((border "1")) (('tr (td "1")) ((td "Song 1")) ((tr ((td "2" ((td "Song 2")))))))))

(check-expect (make-rankings one-list) one-list-web)

(define (make-rankings los)
  (cond [(empty? los) '()]
        [(cons? los)  (make-table (first (ranking los))
                                  (make-rankings (rest los)))]))
 

; List-of-strings -> LoSR
; Consumes a list of strings, and returns a LoSR, with each Song-Ranking containing a number, and a title string. The number strings represent the input LOS order from left to right.
(check-expect (ranking '("Song 1" "Song 2" )) '((1 "Song 1") (2 "Song 2")))
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> LoSR
; Consumes a list of strings, and returns a LoSR. The number for the furthest right title string will be the length of the LOS, and the number string will decrease with each title.
(check-expect (add-ranks '()) '())
(check-expect (add-ranks '("Song 2" "Song 1")) '((2 "Song 2") (1 "Song 1")))
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))
  