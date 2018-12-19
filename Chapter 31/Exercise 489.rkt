;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 489|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define this-step (first l))
                  (define adjusted
                    (map (lambda (x) (+ this-step x)) (relative->absolute (rest l)))))
            (cons (first l) adjusted))]))

(check-expect (relative->absolute/a '(50 40 70 30 30))
              '(50 90 160 190 220))
(define (relative->absolute/a l)
  (local ((define (helper l acc)
            (cond
              [(empty? l) '()]
              [else (local ((define tally (+ (first l) acc)))
                      (cons tally (helper (rest l) tally)))])))
    (helper l 0)))

; N [List-of Function] -> [List Number Number]
; to look for n in (list 0 ... (- n 1))
(define (timing n lof)
  (local ((define long-list
            (build-list n (lambda (x) 50))))
    (map
     (lambda (f)
       (time (first (f long-list))))
     lof)))

(timing 100 `(,relative->absolute ,relative->absolute/a))
(timing 200 `(,relative->absolute ,relative->absolute/a))
(timing 400 `(,relative->absolute ,relative->absolute/a))
(timing 800 `(,relative->absolute ,relative->absolute/a))
(timing 1600`(,relative->absolute ,relative->absolute/a))
(timing 3200 `(,relative->absolute ,relative->absolute/a))


(define (reverse-man l acc)
  (cond
    [(empty? l) acc]
    [else (reverse-man
           (rest l)
           (cons (first l) acc))]))
(reverse-man '(a b c d e f g) '())

(check-expect (reverse-slow '(a b c)) '(c b a))
(define (reverse-slow l)
  (cond
    [(empty? l) '()]
    [else (add-to-end (reverse-slow (rest l)) (first l))]))

(define (add-to-end loi i)
  (cond
    [(empty? loi) (list i)]
    [else (cons (first loi) (add-to-end (rest loi) i))]))