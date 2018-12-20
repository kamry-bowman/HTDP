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
; A history is a [List-of puzzle] representing past states

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
          (define (solve* los a)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else (solve* (create-next-states los))])))
    (solve* (list state0) '())))

(define (final? puzzle)
  (equal? (puzzle-right puzzle)))

(define (create-next-states puzzle)
  (local ((define boat (puzzle-boat puzzle))
          (define left (puzzle-left puzzle))
          (define right (puzzle-right puzzle))
          (define next-history (cons puzzle (puzzle-history puzzle)))
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
                (and (okay? source) (okay? target)))))
    (cond
      [boat (local ((define eligible-states
                      (filter eligible? (generate-states left right)))
                    (define (structure-state state)
                      (make-puzzle (first state) (second state) boat next-history)))
              (map structure-state eligible-states))]
      [else (local ((define eligible-states
                      (filter eligible? (generate-states right left)))
                    (define (structure-state state)
                      (make-puzzle (second state) (first state) boat next-history)))
              (map structure-state eligible-states))])))

(create-next-states initial-puzzle)
    