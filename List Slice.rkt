;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |List Slice|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (slice l s e)
  (cond [(<= (length l) s) '()]
        [(> (length l) e) (slice (rest l) s e)]
        [else (cons (first l) (slice (rest l) s e))]))