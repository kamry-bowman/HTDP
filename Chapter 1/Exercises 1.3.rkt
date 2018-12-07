;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercises 1.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define str "0123456789")

(define
  (string-insert new-str og-str pos)
  (string-append (substring og-str 0 pos)
                 new-str
                 (substring og-str pos (string-length og-str))))

(define
  (string-delete str pos)
  (string-append (substring str 0 pos)
                 (substring str (+ pos 1) (string-length str))))

(string-delete "nuggets" 3)

