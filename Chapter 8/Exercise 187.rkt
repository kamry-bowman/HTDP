;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Exercise 187|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points


; GamePlayer, GamePlayer -> Boolean
; Receives two gameplayers, returns true if the
; first has a higher or equal score than the second
(check-expect (gp>? (make-gp "Kam" 4) (make-gp "Eric" 2)) #true)
(check-expect (gp>? (make-gp "Kam" 2) (make-gp "Eric" 111)) #false)
(define (gp>? gp1 gp2)
  (>= (gp-score gp1) (gp-score gp2)))

; GamePlayer, List-of-gameplayers -> List-of-gameplayers
; Receives a gameplayer, and places it in the list of gameplayers
; based on how score compares to surrounding players, with higher
; players earlier in the list
(check-expect (insert-gp (make-gp "The Best" 3) '()) (list (make-gp "The Best" 3)))
(check-expect (insert-gp (make-gp "The Worst" 1) (list (make-gp "The Best" 3))) (list (make-gp "The Best" 3) (make-gp "The Worst" 1)))
(check-expect (insert-gp (make-gp "The Middle" 2) (list (make-gp "The Best" 3) (make-gp "The Worst" 1))) (list (make-gp "The Best" 3) (make-gp "The Middle" 2) (make-gp "The Worst" 1)))
(define (insert-gp gp lgp)
  (cond [(empty? lgp) (list gp)]
        [(cons? lgp) (if (gp>? gp (first lgp))
                         (cons gp lgp)
                         (cons (first lgp) (insert-gp gp (rest lgp))))]))


; A List-of-GamePlayers (LoGP) is either:
; - '()
; - (cons GamePlayer List-of-GamePlayers
(check-expect (sort-GP '()) '())
(check-expect (sort-GP (list (make-gp "The Best" 3) (make-gp "The Worst" 1))) (list (make-gp "The Best" 3) (make-gp "The Worst" 1)))
(check-expect (sort-GP (list (make-gp "The Worst" 1) (make-gp "The Best" 3))) (list (make-gp "The Best" 3) (make-gp "The Worst" 1)))
(check-expect (sort-GP (list (make-gp "The Best" 3) (make-gp "The Worst" 1) (make-gp "The Middle" 2))) (list (make-gp "The Best" 3) (make-gp "The Middle" 2) (make-gp "The Worst" 1)))
(define (sort-GP lgp)
  (cond [(empty? lgp) '()]
        [(cons? lgp) (insert-gp (first lgp) (sort-GP (rest lgp)))]))