;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 339|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

; A Dir.v3 is a structure
; (make-dir String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

; A File.v3 is a structure:
; (make-file String Number String)

(define date-ex (make-date 15 1 15 11 22 21))
  

(define Text
  (make-dir "Text"
            '()
            `(,(make-file "part1" 99 date-ex  "")
              ,(make-file "part2" 52 date-ex "")
              ,(make-file "part3" 17 date-ex ""))))

(define Libs
  (make-dir "Libs"
            `(,(make-dir "Code"
                         '()
                         `(,(make-file "hang" 8 date-ex "")
                           ,(make-file "draw" 2 date-ex "")))
              ,(make-dir "Docs"
                         '()
                         `(,(make-file "read!" 19 date-ex ""))))
            '()))
(define TS
  (make-dir "TS"
            `(,Libs ,Text)
            `(,(make-file "read!" 10 date-ex ""))))

(define W (create-dir "C:\\Users\\kamdu\\Documents\\Personal-Projects\\Bootstrap clean white\\app"))

; Dir.v2 -> Number
; Determines number of File.v1 within Dir.v2 nested structure
(check-expect (how-many Text) 3)
(check-expect (how-many Libs) 3)
(check-expect (how-many TS) 7)
(define (how-many d)
     (foldr (lambda (sub-d accum)
              (+
               accum
               (how-many sub-d)))
            (length (dir-files d))
            (dir-dirs d)))

; dir file => boolean
; Determines whether file occurs within dir
(check-expect (find? TS "hang") #true)
(check-expect (find? Text "hang") #false)
(define (find? d f-target)
  (local ((define (handle-files fs)
            (ormap (lambda (sub-file)
                           (string=? (file-name sub-file) f-target))
                   fs)))
       (ormap (lambda (sub-d)
                (or (handle-files (dir-files sub-d))
                    (find? sub-d f-target)))
            (dir-dirs d))))
