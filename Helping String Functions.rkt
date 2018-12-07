;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Helping String Functions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;String->String
;Returns the first character of a string
;(check-expect (string-first "") "")
;(check-expect (string-first "knight") "k")
(define (string-first str)
  (cond [(equal? str "") ""]
        [else (string-ith str 0)]))

;String->String
;Returns the last character of a string
;(check-expect (string-last "") "")
;(check-expect (string-last "knight") "t")
(define (string-last str)
    (cond [(equal? str "") ""]
        [else (substring str (- (string-length str) 1))]))

;String, String, Number -> String
;Returns the first string, with the second string inserted as index Number
;(check-expect (string-insert "01234" "+" 2) "01+234")
;(check-expect (string-insert "01234" "+" 0) "+01234")
;(check-expect (string-insert "01234" "+" 5) "01234+")
;(check-expect (string-insert "01234" "+" 50) "01234")
(define (string-insert str1 str2 i)
  (if (>= (- (string-length str1) i) 0)
      (string-append (substring str1 0 i) str2 (substring str1 i))
      str1))

;String, Number -> String
;Consumeses a string, and returns a string with character at ith place deleted
;If -i is passsed, the string is returned wit the index starting from the end
;(check-expect (string-delete "0123456" 3) "012456")
;(check-expect (string-delete "0123456" 8) "0123456")
;(check-expect (string-delete "" 5) "")
;(check-expect (string-delete "0123456" -1) "012345")
;(check-expect (string-delete "0123456" -2) "01234")
;(check-expect (string-delete "0123456" -8) "")

(define (string-delete str i)
  (cond [(equal? str "") ""]
        [(< i 0) (substring str 0 (if (>= (+ (string-length str) i) 0)
                                       (+ (string-length str) i)
                                       0))]
        [(> i (string-length str)) str]
        [else (string-append (substring str 0 i) (substring str (+ i 1)))]))

;String, Number->String
;(string-slicer s n) takes a string, and takes the first n characters if n is positive
;if n is negative takes the last abs(n) character
;(check-expect (string-slicer "0123456" 1) "0")
;(check-expect (string-slicer "0123456" 3) "012")
;(check-expect (string-slicer "0123456" -1) "6")
;(check-expect (string-slicer "0123456" -3) "456")
;(check-expect (string-slicer "0123456" 0) "")
;(check-expect (string-slicer "0123456" 8) "0123456")
;(check-expect (string-slicer "0123456" -8) "0123456")
;(check-expect (string-slicer "" 5) "")

(define (string-slicer str num)
  (cond [(equal? str "") ""]
        [(< num 0) (substring str
                            (if (<= 0 (+ (string-length str) num))
                                (+ (string-length str) num)
                                0)
                            (string-length str))]
        [(> num (string-length str)) str]
        [else (substring str 0 num)]))


  
   