;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Exercise 343|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Path is [List-of String].
; interpretation directions into a directory tree

(define Text
  (make-dir "Text"
            '()
            `(,(make-file "part1" 99 date-ex  "")
              ,(make-file "part2" 52 date-ex "")
              ,(make-file "part3" 17 date-ex ""))))

(define Libs
  (make-dir "Libs"
            `(,(make-dir "Code"
                         `(,(make-dir "Docs"
                         '()
                         `(,(make-file "read!" 19 date-ex ""))))
                         `(,(make-file "hang" 8 date-ex "")
                           ,(make-file "draw" 2 date-ex "")))
              ,(make-dir "Docs"
                         '()
                         `(,(make-file "read!" 19 date-ex "")))
              ,(make-dir "Empty"
                         '()
                         '()))
            '()))
(define TS
  (make-dir "TS"
            `(,Libs ,Text)
            `(,(make-file "read!" 10 date-ex ""))))

;(define W (create-dir "C:\\Users\\kamdu\\Documents\\Personal-Projects\\Bootstrap clean white\\app"))

; Dir.v3 -> Path
; Determines number of File.v1 within Dir.v2 nested structure
(check-expect (find-all Text) #false)
(check-expect (find-all TS) 0)
(define (find-all d)
  (local
    ; Processes a directory
    ; Dir -> [Maybe Path]
    ((define (handle-dir d)
       (append (map (lambda (f)
                      (list (file-name f)))
                    (dir-files d))
               (handle-dirs (dir-dirs d))))
     ; Processes a list of directories, and returns false
     ; if empty, or the file is not anywhere within any of the
     ; dirs contained within.
     ; Dir* -> 
     (define (handle-dirs ds)
       (cond [(empty? ds) '()]
             [else (append (map (lambda (result-path)
                                      (cons (dir-name (first ds))
                                                result-path
                                            ))
                                    (handle-dir (first ds)))
                               (handle-dirs (rest ds)))])))
     (handle-dir d)))
             
       

