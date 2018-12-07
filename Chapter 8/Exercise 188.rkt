;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Exercise 188|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

; A List of Emails is a structure, either:
;  -'()
;  -(cons email List-of-emails)

; Email, Email -> Boolean
; Returns true if first email is more recent or the same time stamp as the second email, else false
(check-expect (newer-email? old-email new-email) #false)
(check-expect (newer-email? new-email old-email) #true)
(define (newer-email? e1 e2)
  (<= (email-date e1) (email-date e2)))

;Email, List-of-email
;Adds email between the most recent and the least old relative to it
(check-expect (insert-email old-email '()) (list old-email))
(check-expect (insert-email new-email (list old-email)) (list new-email old-email))
(check-expect (insert-email old-email (list new-email)) (list new-email old-email))
(check-expect (insert-email recent-email (list new-email old-email)) (list new-email recent-email old-email))
(define (insert-email e loe)
  (cond [(empty? loe) (list e)]
        [(cons? loe) (if (newer-email? e (first loe))
                         (cons e loe)
                         (cons (first loe) (insert-email e (rest loe))))]))

;List-of-email->List-of-email
;Returns a list of email sorted most recent to oldest
(define old-email (make-email "j@j.com" 10 "hi"))
(define recent-email (make-email "k@j.com" 5 "hi"))
(define new-email (make-email "l@j.com" 0 "hi"))

(check-expect (sort-email '()) '())
(check-expect (sort-email (list old-email))
               (list old-email))
(check-expect (sort-email (list new-email old-email)) (list new-email old-email))
(check-expect (sort-email (list old-email new-email)) (list new-email old-email))
(check-expect (sort-email (list old-email recent-email new-email)) (list new-email recent-email old-email))

(define (sort-email loe)
  (cond [(empty? loe) '()]
        [(cons? loe) (insert-email (first loe) (sort-email (rest loe)))]))