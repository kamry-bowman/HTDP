;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 471|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Node is a Symbol.

; A Graph is one-of
; - '()
; - (cons '(Node [List-of Node]) Graph)

(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

; Node Graph -> [List-of Node]
; Returns a List-of Nodes representing
; n's immediate neighbors in g
(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'D sample-graph) '())
(check-expect (neighbors 'F sample-graph) '(D G))

(define (neighbors n g)
  (second (assq n g)))

; Node Node Graph -> [List-of Node]
; finds a path from origination to destination in G
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local
            ((define next (neighbors origination G))
             (define candidate
               (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

; A Path is a [List-of Node].
; interpretation: the list of nodes specifies a sequence
; of immediate neighbors that leads from the first
; Node on the list to the last one.

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if tehre is no path, the function produces #false

(check-expect (find-path 'C 'D sample-graph)
              '(C D))

(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))

(check-expect (find-path 'C 'G sample-graph)
              #false)


; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-originations to
; destination; otherwise, it produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define all-acceptable
                    (foldr (lambda (candidate acc)
                             (local ((define result (find-path candidate D G)))
                               (if (boolean? result) acc (cons result acc))))
                           '()
                           lo-Os)))
            (cond
              [(empty? all-acceptable) #false]
              [else (first all-acceptable)]))]))

(find-path 'A 'G sample-graph)


; Graph -> Boolean
; consumes a graph and determines whether there is a path between any pair
; of nodes
(check-expect (test-on-all-nodes '((A ()))) #false)
(check-expect (test-on-all-nodes '((A (B)) (B ()))) #true)
(check-expect (test-on-all-nodes '((A (B C) (D (E)) (F ())))) #false)
(check-expect (test-on-all-nodes '((A (B C)) (D (E)) (F (A)))) #true)

(define (test-on-all-nodes G)
  (local ((define (test-list loe)
            (cond
              [(empty? loe) #false]
              [(connects? (second (first loe)) G) #true]
              [else (test-list (rest loe))])))
    (test-list G)))

; [List-of Node] Graph -> Boolean
; Determines whether any Node in List-of Node is in Graph
(define (connects? loe G)
  (cond
    [(empty? loe) #false]
    [(cons? (assq (first loe) G)) #true]
    [else (connects? (rest loe) G)]))

(define cyclic-graph '((A (B E)) (B (E F)) (E (C F)) (C (B D)) (F (D G)) (D ()) (G ())))

; (find-path 'B 'C cyclic-graph)
(test-on-all-nodes cyclic-graph)
