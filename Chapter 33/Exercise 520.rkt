;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 520|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a group is a pair:
; (list missionaries cannibals)
; where missionaries and cannibals
; are numbers representing number of
; missionaries and cannibals

;a puzzle state is a structure
(define-struct puzzle [left right boat history])
; a left and right is pair
; Boat is a Boolean, with true meaning left bank
; false meaning right bank.
; A history is a [List-of (list left right boat)] representing past states

(define initial-puzzle (make-puzzle '(3 3) '(0 0) #true '()))
(define final-puzzle (make-puzzle '(0 0) '(3 3) #false '()))

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative: creates a tree of possible boat rides
; termination

;(check-expect (solve initial-puzzle) final-puzzle)

(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative: generates the successors of los
          ; accumulator: represents the states that have been moved through
          ; since state0 
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else (solve* (create-next-states los))])))
    (solve* (list state0))))

(define (final? puzzle)
  (equal? (puzzle-right puzzle) '(3 3)))

(define (create-next-states los)
  (local ((define (create-for-one puzzle)
            (local ((define boat (puzzle-boat puzzle))
                    (define left (puzzle-left puzzle))
                    (define right (puzzle-right puzzle))
                    (define history (puzzle-history puzzle))
                    (define next-history (cons (list left right boat) history))
                    (define (generate-states source target)
                      (local ((define mis-source (first source))
                              (define can-source (second source))
                              (define mis-target (first target))
                              (define can-target (second target))
                              (define (generate/a n states)
                                (cond
                                  [(= n -1) states]
                                  [(= n 0) (generate/a
                                            (- n 1)
                                            (if (> mis-source 1)
                                                (cons `((,(- mis-source 2) ,can-source)
                                                        (,(+ mis-target 2) ,can-target))
                                                      states)
                                                states))]
                                  [(= n 1) (generate/a
                                            (- n 1)
                                            (if (> can-source 1)
                                                (cons `((,mis-source ,(- can-source 2))
                                                        (,mis-target ,(+ can-target 2)))
                                                      states)
                                                states))]
                                  [(= n 2) (generate/a
                                            (- n 1)
                                            (if (and (>= can-source 1)
                                                     (>= mis-source 1))
                                                (cons `((,(- mis-source 1) ,(- can-source 1))
                                                        (,(+ mis-target 1) ,(+ can-target 1)))
                                                      states)
                                                states))]
                                  [(= n 3) (generate/a
                                            (- n 1)
                                            (if (>= can-source 1)
                                                (cons `((,mis-source ,(- can-source 1))
                                                        (,mis-target ,(+ can-target 1)))
                                                      states)
                                                states))]
                                  [(= n 4) (generate/a
                                            (- n 1)
                                            (if (>= mis-source 1)
                                                (cons `((,(- mis-source 1) ,can-source)
                                                        (,(+ mis-target 1) ,can-target))
                                                      states)
                                                states))])))
                        (generate/a 4 '())))
                    (define (eligible? state)
                      (local ((define source (first state))
                              (define target (second state))
                              (define (okay? pop)
                                (or (= (first pop) 0)
                                    (>= (first pop) (second pop)))))
                        (and (okay? source) (okay? target))))
                    (define eligible-candidates
                      (cond
                        [boat (local ((define eligible-states
                                        (filter eligible? (generate-states left right)))
                                      (define (structure-state state)
                                        (make-puzzle (first state) (second state) (not boat) next-history)))
                                (map structure-state eligible-states))]
                        [else (local ((define eligible-states
                                        (filter eligible? (generate-states right left)))
                                      (define (structure-state state)
                                        (make-puzzle (second state) (first state) (not boat) next-history)))
                                (map structure-state eligible-states))]))
                    (define (find-new candidates)
                      (filter (lambda (candidate)
                                (andmap (lambda (historic)
                                          (not (and (equal? (first historic) (puzzle-left candidate))
                                                    (equal? (second historic) (puzzle-right candidate))
                                                    (equal? (third historic) (puzzle-boat candidate)))))
                                        next-history))
                              candidates)))
              (find-new eligible-candidates))))
  (foldr (lambda (puzzle acc)
           (local ((define candidates
                     (create-for-one puzzle))
                   (define (check-distinct candidate)
                     (andmap (lambda (alternate)
                               (not (and (equal? (puzzle-left alternate) (puzzle-left candidate))
                                         (equal? (puzzle-right alternate) (puzzle-right candidate))
                                         (equal? (puzzle-boat alternate) (puzzle-boat candidate)))))
                             acc)))
             (append (filter check-distinct candidates) acc))) '() los)))

(create-next-states (list initial-puzzle))
(create-next-states (list
 (make-puzzle (list 3 1) (list 0 2) #false (list (list (list 3 3) (list 0 0) #true)))
 (make-puzzle (list 2 2) (list 1 1) #false (list (list (list 3 3) (list 0 0) #true)))
 (make-puzzle (list 3 2) (list 0 1) #false (list (list (list 3 3) (list 0 0) #true)))))
(create-next-states (list (make-puzzle (list 3 2) (list 0 1) #true (list (list (list 2 2) (list 1 1) #false) (list (list 3 3) (list 0 0) #true)))))


(solve initial-puzzle)
    