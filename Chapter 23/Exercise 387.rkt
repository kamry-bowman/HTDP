;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 387|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [else
     (cons (first front) (replace-eol-with (rest front) end))]))

(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a)) (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a)) (cons 2 (cons 1 '(a))))

; [List-of Symbols] [List-of Numbers] -> [List-of (list Symbol Number)]
; Consumes a List-of Symbols and a List-of Number and returns a List-of ordered
; pairs representing all possible Symbol and Number combinations
(define (cross list1 list2)
  (local (; [Symbol [List-of Number] => [List-of (list Symbol Number)]
          (define (cross-symbol sym l)
            (cond [(empty? l) '()]
                  [else (cons (list sym (first l)) (cross-symbol sym (rest l)))])))
    (cond [(empty? list1) '()]
          [else (replace-eol-with (cross-symbol (first list1) list2) (cross (rest list1) list2))])))

(check-expect (cross '() '(1)) '())
(check-expect (cross '(a) '()) '())
(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b) '(1 2)) '((a 1) (a 2) (b 1) (b 2)))
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))