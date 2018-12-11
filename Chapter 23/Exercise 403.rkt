;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 403|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)
 
; A Schema is a [List-of Spec]
(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)
; A Label is a String
; A Predicate is a [Any -> Boolean]

 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-content
  '(("Alice" 35 #true)
           ("Bob" 25 #false)
           ("Carol" 30 #true)
           ("Dave" 32 #false)))

(define school-schema
  (list (make-spec "Name" string?) (make-spec "Age" integer?) (make-spec "Present" boolean?)))

(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  '((#true "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema presence-content))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
 
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check (make-db school-schema presence-content)) #false)
 
(define (integrity-check db)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define width (length schema))
          ; Row -> Boolean
          (define (row-integrity-check row)
            (and (= (length row) width)
                 (andmap2 row (map spec-predicate schema))
                 )))
    (andmap row-integrity-check content)))

; [List-of Any] [List-of (list String Any->Boolean)] => Boolean
(define (andmap2 l lop)
  (cond
    [(empty? l) #true]
    [((first lop) (first l)) (andmap2 (rest l) (rest lop))]
    [else #false]))

(check-expect (andmap2 '(#true "Henry") `(,boolean? ,string?)) #true)
(check-expect (andmap2 '(1 "Henry") `(,boolean? ,string?)) #false)

; DB [List-of Label] -> DB
; retains a column from db if its label is in labels
(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define schema-labels (map spec-label schema))
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (spec-label c) labels))
          ; Row [List-of Label] -> Row
          ; retains those cells whose corresponding element
          ; in names is also in labels
          (define (row-filter row names)
            (cond
              [(empty? row) '()]
              [(member? (first names) labels) (cons (first row) (row-filter (rest row) (rest names)))]
              [else (row-filter (rest row) (rest names))]))
          ; Row -> Row
          (define (row-project row)
            (foldr (lambda (cell include-this? acc)
                     (if include-this?
                         (cons cell acc)
                         acc))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema) (map row-project content))))

(define projected-content
  `(("Alice" #true)
    ("Bob" #false)
    ("Carol" #true)
    ("Dave" #false)))

(define projected-schema
  `(,(make-spec "Name" string?) ,(make-spec "Present" boolean?)))

(define projected-db
  (make-db projected-schema projected-content))

(check-expect (db-content (project school-db '("Name" "Present")))
              (db-content projected-db))

; db [List-of Label] [Any->Boolean] -> [List-of Row]
; Consumes a database, filters it by predicate, then returns
; a projection including only columns matching lol labels
(define (select db lol predicate)
  (db-content (project (make-db (db-schema db) (filter predicate (db-content db))) lol)))

(check-expect (select school-db `("Name" "Present") (lambda (row) (<= (second row) 30))) '(("Bob" #false) ("Carol" #true)))

