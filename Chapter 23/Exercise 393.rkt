;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 393|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Set Set -> Set
(define (union s1 s2)
  (local ((define (filter t l)
            (cond [(empty? l) l]
                  [(eq? (first l) t) (filter t (rest l))]
                  [else (cons (first l) (filter t (rest l)))])))
  (cond
    [(empty? s1) s2]
    [else (cons (first s1) (union (rest s1) (filter (first s1) s2)))])))

(define s0 '(1 2))
(define s1 '(2 3))
(check-expect (union s0 s1) '(1 2 3))

(define (intersect s1 s2)
  (local ((define (find t l)
            (cond
              [(empty? l) #false]
              [(eq? t (first l)) #true]
              [else (find t (rest l))])))
  (cond [(empty? s1) '()]
        [(find (first s1) s2) (cons (first s1) (intersect (rest s1) s2))]
        [else (intersect (rest s1) s2)])))
        

(check-expect (intersect s0 s1) '(2))