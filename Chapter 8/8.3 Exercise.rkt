;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |8.3 Exercise|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-names is one of: 
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name


; List-of-names -> Boolean
; determines whether "Flatt" is on a-list-of-names
(define (contains? name a-list-of-names)
  (cond [(empty? a-list-of-names) #false]
        [(cons? a-list-of-names) (if (equal? (first a-list-of-names) name)
                                             #true
                                             (contains? name (rest a-list-of-names)))]))


(check-expect (contains? "Flatt" '()) #false)
(check-expect (contains? "Flatt" (cons "Find" '()))
              #false)
(check-expect (contains? "Flatt" (cons "Flatt" '()))
              #true)
(check-expect
 (contains? "Flatt"
  (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)

(check-expect
 (contains? "Flatt"
  (cons "A" (cons "B" (cons "C" '()))))
 #false)


(contains? "Flatt"
  (cons "A" (cons "Flatt" (cons "C" '()))))