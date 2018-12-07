;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Exercise 189|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number, List-of-sorted-numbers -> Boolean
; Determines whether the Number is contained in the list-of-sorted-numbers
(check-expect (sorted-search 5 '()) #false)
(check-expect (sorted-search 5 (list 5)) #true)
(check-expect (sorted-search 5 (list 4)) #false)
(check-expect (sorted-search 5 (list 6 4)) #false)
(check-expect (sorted-search 5 (list 6 5 4)) #true)
(define (sorted-search number sl)
  (cond [(empty? sl) #false]
        [(cons? sl) (cond [(= number (first sl)) #true]
                          [(> number (first sl)) #false]
                          [(< number (first sl)) (sorted-search number (rest sl))])]))
                    
