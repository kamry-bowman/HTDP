;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Exercise 190|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;List-of-1Strings (Lo1s) is a list that consists of either
; -'()
; -(cons 1String Lo1s)

;Lolo1s is a lost of list-of-1Strings that consists of either
; -'()
; -(cons Lo1s Lolo1s)


;Lo1S -> Lo1S
;Consumes a list of 1Strings with at least one element beyond '(), containing all but the first list item after '()
(check-error (front '()) "Need at least one additional 1String beyond empty set")
(check-expect (front (list "a")) '())
(check-expect (front (list "a" "b")) (list "a"))
(check-expect (front (list "a" "b" "c")) (list "a" "b"))
(define (front l)
  (cond [(empty? l) (error "Need at least one additional 1String beyond empty set")]
        [(empty? (rest l)) '()]
        [(cons? (rest l)) (cons (first l) (front (rest l)))]))

;Lo1S -> ;LoLo1s
;Returns a list of all prefixes for a list. A list p is a prefix of l if p and l are same up through all items in p. For example, (list "a" "b" "c") is a prefix of itself and (list "a" "b" "c" "d")
(check-expect (prefix '()) '())
(check-expect (prefix (list "a")) (list (list "a")))
(check-expect (prefix (list "a" "b")) (list (list "a" "b") (list "a")))
(check-expect (prefix (list "a" "b" "c")) (list (list "a" "b" "c") (list "a" "b") (list "a")))
(define (prefix l)
  (cond [(empty? l) '()]
        [(cons? l) (cons l (prefix (front l)))]))

;Lo1S -> LoLo1s
;Returns a list of all suffixes for a list. A list s is a suffix of l if s and l are the same from the end, up through all the items in s. For example, (list "b" "c" "d") is a suffix of itself and (list "a" "b" "c" "d")
(check-expect (suffix '()) '())
(check-expect (suffix (list "a")) (list (list "a")))
(check-expect (suffix (list "a" "b")) (list (list "a" "b") (list "b")))
(check-expect (suffix (list "a" "b" "c")) (list (list "a" "b" "c") (list "b" "c") (list "c")))

(define (suffix l)
  (cond [(empty? l) '()]
        [(cons? l) (cons l (suffix (rest l)))]))

  