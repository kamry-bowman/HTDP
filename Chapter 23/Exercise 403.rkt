;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Exercise 403|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


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

; db db => db
; Consumes two database with identical schemas, and returns a single
; database with the combined rows. Any duplicates are only included once.
(define (db-union db1 db2)
  (local ((define content1 (db-content db1))
          (define content2 (db-content db2))
          (define schema (db-schema db1))
          ; row row -> Boolean
          ; Determines if rows are unique
          (define (row-diff? row1 row2)
            (not (andmap eq? row1 row2)))
          ; row [List-of rows] -> Boolean
          ; Determines if a row1 is included in rows
          (define (row-unique? row1 rows)
            (andmap (lambda (row) (row-diff? row1 row)) rows))
          (define uniques-2
            (filter (lambda (row) (row-unique? row content1)) content2)))
    (make-db schema (append content1 uniques-2))))

(define school-db2 (make-db school-schema
                            '(("John" 42 #true)
                              ("Bob" 25 #false))))

(define school-db-union (make-db school-schema
                                 '(("Alice" 35 #true)
                                   ("Bob" 25 #false)
                                   ("Carol" 30 #true)
                                   ("Dave" 32 #false)
                                   ("John" 42 #true))))
(check-expect (db-union school-db school-db2) school-db-union)

; [List-of X] [Natural Number] -> [List-of X]
; Returns the items from list n from end
(define (split-right lst n)
  (local ((define (build-up lst cache n)
            (cond
              [(= n 0) `(,(reverse lst) ,cache)]
              [(empty? lst) (error "list too short")]
              [else (build-up (rest lst) (cons (first lst) cache) (sub1 n))])))
    (build-up (reverse lst) '() n)))
    
(check-expect (split-right '() 0) '(() ()))
(check-expect (split-right '(a) 0) '((a) ()))
(check-expect (split-right '(a) 1) '(() (a)))
(check-expect (split-right '(a b) 1) '((a) (b)))
(check-expect (split-right '(a b c) 1) '((a b) (c)))
(check-expect (split-right '(a b c) 2) '((a) (b c)))

; db db => db
; Accepts two dbs, and performs a join on them, using the
; last row of the first db, and the first row of the second db
; as the join column
(define (join db-1 db-2)
  (local ((define schema-1 (db-schema db-1))
          (define schema-2 (db-schema db-2))
          (define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          (define schema-1-split (split-right schema-1 1))
          (define new-schema (append (first (split-right schema-1 1)) (rest schema-2)))
          (define (grab-rows cell)
            (foldr (lambda (row acc) (if (eq? (first cell) (first row))
                                         (cons (rest row) acc)
                                         acc))
                   '()
                   content-2))
          (define (process-row left-row acc)
            (local ((define split-left-row (split-right left-row 1)))
              (append
               (map (lambda (row) (append (first split-left-row) row))
                    (grab-rows (second split-left-row)))
               acc)))
          (define new-content
            (foldr process-row '() content-1)))
    (make-db new-schema new-content)))


(define joinEx0 (make-db
                 `(,(make-spec "Name" string?)
                   ,(make-spec "Age" integer?)
                   ,(make-spec "Description" string?))
                 '(("Alice" 35 "presence")
                   ("Bob" 25 "absence")
                   ("Carol" 30 "presence")
                   ("Dave" 32 "absence"))))

(define db-presence-ext (make-db
                         `(,(make-spec "Present" boolean?)
                           ,(make-spec "Description" string?)
                           ,(make-spec "Points" number?))
                         `((#true "presence" 1) (#false "absence" -2))))

(define db-presence-ext-2 (make-db
                         `(,(make-spec "Present" boolean?)
                           ,(make-spec "Description" string?)
                           ,(make-spec "Points" number?))
                         `((#true "presence" 1) (#true "here" 1) (#false "absence" -2) (#false "out" -2))))

(define joinEx1 (make-db
                 `(,(make-spec "Name" string?)
                   ,(make-spec "Age" integer?)
                   ,(make-spec "Description" string?)
                   ,(make-spec "Points" number?))
                 '(("Alice" 35 "presence" 1)
                   ("Bob" 25 "absence" -2)
                   ("Carol" 30 "presence" 1)
                   ("Dave" 32 "absence" -2))))

(define joinEx2 (make-db
                 `(,(make-spec "Name" string?)
                   ,(make-spec "Age" integer?)
                   ,(make-spec "Description" string?)
                   ,(make-spec "Points" number?))
                 '(("Alice" 35 "presence" 1)
                   ("Alice" 35 "here" 1)
                   ("Bob" 25 "absence" -2)
                   ("Bob" 25 "out" -2)
                   ("Carol" 30 "presence" 1)
                   ("Carol" 30 "here" 1)
                   ("Dave" 32 "absence" -2)
                   ("Dave" 32 "out" -2))))
   
(check-expect (db-content (join school-db presence-db)) (db-content joinEx0))
(check-expect (db-content (join school-db db-presence-ext)) (db-content joinEx1))
(check-expect (db-content (join school-db db-presence-ext-2)) (db-content joinEx2))

(db-schema (join school-db db-presence-ext))
  