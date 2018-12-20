;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 512|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

(define ex5 '((λ (x) x) (λ (x) x)))
(define ex6 '(((λ (y) (λ (x) y)) (λ (z) z)) (λ (w) w)))

; Lam -> Boolean
; Determines whether an expression is a var
(define (is-var? ex)
  (symbol? ex))
(check-expect (is-var? 'x) #true)
(check-expect (is-var? ex1) #false)

; Lam -> Boolean
; determines whether an expression is a lambda
(define (is-lambda? ex)
  (and
   (cons? ex)
   (= (length ex) 3)
   (symbol? (first ex))
   (symbol=? (first ex) 'λ)
   (cons? (second ex))
   (andmap is-var? (second ex))
   (is-lam? (third ex))))
(check-expect (is-lambda? ex1) #true)
(check-expect (is-lambda? 'x) #false)
(check-expect (is-lambda? ex3) #true)
(check-expect (is-lambda? ex5) #false)
(check-expect (is-lambda? ex6) #false)

; Lam -> Boolean
; determines whether an expression is an app
(define (is-app? ex)
  (and
   (cons? ex)
   (= (length ex) 2)
   (is-lam? (first ex))
   (is-lam? (second ex))))

(check-expect (is-app? ex1) #false)
(check-expect (is-app? 'x) #false)
(check-expect (is-app? ex5) #true)
(check-expect (is-app? ex6) #true)

(define (is-lam? ex)
  (or
   (is-var? ex)
   (is-lambda? ex)
   (is-app? ex)))

(check-expect (is-lam? ex1) #true)
(check-expect (is-lam? ex2) #true)
(check-expect (is-lam? ex3) #true)
(check-expect (is-lam? ex4) #true)
(check-expect (is-lam? ex5) #true)
(check-expect (is-lam? ex6) #true)
(check-expect (is-lam? 'x) #true)
(check-expect (is-lam? '(x x)) #true)
(check-expect (is-lam? '(x x x)) #false)
(check-expect (is-lam? '(λ x x)) #false)
(check-expect (is-lam? '(λ (x) x x)) #false)
(check-expect (is-lam? '(x x x)) #false)

(define (λ-para ex)
  (second ex))

(define (λ-body ex)
  (third ex))

(define (app-fun ex)
  (first ex))

(define (app-arg ex)
  (second ex))

; Lam -> Lam 
; replaces all symbols s in le with '*undeclared
; if they do not occur within the body of a λ 
; expression whose parameter is s

(check-expect (undeclareds ex1) ex1)
(check-expect (undeclareds ex2) '(λ (x) *undeclared))
(check-expect (undeclareds ex3) ex3)
(check-expect (undeclareds ex4) ex4)
 
(define (undeclareds le0)
  (local
    (; Lam ??? -> Lam
     ; accumulator: declareds is a list of all λ
     ; parameters on the path from le0 to le
     (define (undeclareds/a le declareds)
       (cond
         [(is-var? le)
          (if (member? le declareds) le '*undeclared)]
         [(is-lambda? le)
          (local ((define para (λ-para le))
                  (define body (λ-body le))
                  (define newd (append para declareds)))
            (list 'λ para
                  (undeclareds/a body newd)))]
         [(is-app? le)
          (local ((define fun (app-fun le))
                  (define arg (app-arg le)))
            (list (undeclareds/a fun declareds)
                  (undeclareds/a arg declareds)))])))
  (undeclareds/a le0 '())))