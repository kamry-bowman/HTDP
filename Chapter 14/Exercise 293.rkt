;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 293|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x)
         l
         (find x (rest l)))]))

; X [List-of X] -> [Maybe[List-of X] -> Boolean]
; Returns a function that accepts a Maybe(list-of x), and determines whether it is a sublist of list-of x argument that starts with arguments x, or is otherwise false
(define (found x lox)
  (lambda (s-candidate)
    (local ( ; Maybe[List-of-X] [List-of-X] -> Boolean
             ; Determines whether a list-of x (s-candidate) is a substring
             ; starting of [List-of X] (lox) in
             ; parent function. Returns true if lox is empty and s-candidate is true
            (define (found/x-bound lox s-candidate)
              (cond [(empty? lox) (boolean=? s-candidate #false)]
                    [(empty? s-candidate) #false]
                    [(not (equal? x (first s-candidate))) #false]
                    [else (if (equal? x (first lox))
                              (local (; [NEList-of X] [NEList-of X] -> Boolean
                                      ; Determines whether two NElist-of x contain the same x in the same order
                                      (define (same-list? s-candidate lox)
                                        (cond [(empty? (rest s-candidate)) (and (empty? (rest lox)) (equal? (first s-candidate) (first lox)))]
                                              [else (if (equal? (first s-candidate) (first lox))
                                                        (same-list? (rest lox) (rest s-candidate))
                                                        #false)])))
                                (same-list? lox s-candidate))
                              (found/x-bound s-candidate (rest lox)))])))                                             
      (found/x-bound lox s-candidate))))

((found 2 '(1 2 3)) '(2 3 3))

;(check-satisfied (find 2 '(1 2 3 4 5 6)) (found 2 '(1 2 3 4 5 6)))