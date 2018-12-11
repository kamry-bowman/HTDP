;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 425|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else (local ((define pivot (first alon)))
                  (append (quick-sort< (smallers (rest alon) pivot))
                          (list pivot)
                          (quick-sort< (largers(rest alon) pivot))))]))

(check-expect (quick-sort< '(3 4 9 1 8 7 6)) '(1 3 4 6 7 8 9))

; [List-of Number] Number -> [List-of Number]
; Returns a list that includes all numbers smaller than pivot
(define (smallers lon pivot)
  (cond [(empty? lon) '()]
        [(< (first lon) pivot) (cons (first lon) (smallers (rest lon) pivot))]
        [else (smallers (rest lon) pivot)]))

(check-expect (smallers '(3 2 7 8 9 3) 5) '(3 2 3))


; [List-of Number] Number -> [List-of Number]
; Returns a list that includes all numbers equal to or greater than pivot
(define (largers lon pivot)
  (cond [(empty? lon) '()]
        [(>= (first lon) pivot) (cons (first lon) (largers (rest lon) pivot))]
        [else (largers (rest lon) pivot)]))

(check-expect (largers '(3 2 7 8 9 3) 5) '(7 8 9))

