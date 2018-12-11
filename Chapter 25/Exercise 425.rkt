;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 425|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else (local ((define pivot (first alon))
                  (define (pivot-check n)
                    (> n pivot))
                  (define small-large (split (rest alon) pivot-check)))
            (append (quick-sort< (first small-large))
                    (list pivot)
                    (quick-sort< (second small-large))))]))

(check-expect (quick-sort< '(3 4 9 1 8 7 6)) '(1 3 4 6 7 8 9))

; [List-of X] [X->Boolean] -> '(,[List-of X] ,[List-of X])
; Returns a list of two lists, with all X that evaluate as false in first list,
; and all that evaluate as true in the second
(define (split lon predicate)
  (local ((define (splitter lon false-l true-l)
            (cond [(empty? lon) `(,false-l ,true-l)]
                  [(predicate (first lon)) (splitter (rest lon) false-l (cons (first lon) true-l))]
                  [else (splitter (rest lon) (cons (first lon) false-l) true-l)])))
    (splitter lon '() '())))
    
  

