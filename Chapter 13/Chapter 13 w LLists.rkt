;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Chapter 13 w LLists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)


; modify the following to use your chosen name
(define ITUNES-LOCATION "iTunes Music Library.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

(define Girltalktrack1
 (list
  (list "Track ID" 86)
  (list "Name" "Play Your Part (Pt. 1)")
  (list "Artist" "Girl Talk")
  (list "Album" "Feed The Animals")
  (list "Genre" "Mash-up")
  (list "Kind" "MPEG audio file")
  (list "Size" 19612331)
  (list "Total Time" 284865)
  (list "Track Number" 1)
  (list "Track Count" 14)
  (list "Year" 2008)
  (list "Date Modified" (create-date 2008 12 7 18 55 32))
  (list "Date Added" (create-date 2011 2 27 21 45 44))
  (list "Bit Rate" 320)
  (list "Sample Rate" 44100)
  (list "Part Of Gapless Album" #true)
  (list "Comments" "released on Illegal Art")
  (list "Play Count" 1)
  (list "Play Date" 3381673926)
  (list "Play Date UTC" (create-date 2011 2 27 21 52 6))
  (list "Artwork Count" 1)
  (list "Persistent ID" "5E392CBEF8E9ED1F")
  (list "Track Type" "File")
  (list
   "Location"
   "file://localhost/Users/z/Music/iTunes/iTunes%20Media/Music/Girl%20Talk/Feed%20The%20Animals/01%20Play%20Your%20Part%20(Pt.%201).mp3")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1)))

(define Girltalktrack2
  (list
   (list "Track ID" 90)
   (list "Name" "Still Here")
   (list "Artist" "Girl Talk")
   (list "Album" "Feed The Animals")
   (list "Genre" "Mash-up")
   (list "Kind" "MPEG audio file")
   (list "Size" 17704335)
   (list "Total Time" 237165)
   (list "Track Number" 3)
   (list "Track Count" 14)
   (list "Year" 2008)
   (list "Date Modified" (create-date 2008 12 7 18 55 35))
   (list "Date Added" (create-date 2011 2 27 21 45 44))
   (list "Bit Rate" 320)
   (list "Sample Rate" 44100)
   (list "Part Of Gapless Album" #true)
   (list "Comments" "released on Illegal Art")
   (list "Artwork Count" 1)
   (list "Persistent ID" "8780A3C7A0117B2B")
   (list "Track Type" "File")
   (list
    "Location"
    "file://localhost/Users/z/Music/iTunes/iTunes%20Media/Music/Girl%20Talk/Feed%20The%20Animals/03%20Still%20Here.mp3")
   (list "File Folder Count" 5)
   (list "Library Folder Count" 1)))

(define Bushtrack1
  (list
   (list "Track ID" 138)
   (list "Name" "Everything Zen")
   (list "Artist" "Bush")
   (list "Album" "Sixteen Stone")
   (list "Genre" "Rock")
   (list "Kind" "MPEG audio file")
   (list "Size" 5598846)
   (list "Total Time" 278204)
   (list "Disc Number" 1)
   (list "Track Number" 1)
   (list "Year" 1994)
   (list "Date Modified" (create-date 2006 11 26 14 52 45))
   (list "Date Added" (create-date 2011 2 27 21 45 46))
   (list "Bit Rate" 160)
   (list "Sample Rate" 44100)
   (list "Play Count" 1)
   (list "Play Date" 3381673889)
   (list "Play Date UTC" (create-date 2011 2 27 21 51 29))
   (list "Artwork Count" 1)
   (list "Persistent ID" "1EB008B136052926")
   (list "Track Type" "File")
   (list
    "Location"
    "file://localhost/Users/z/Music/iTunes/iTunes%20Media/Music/Bush/Sixteen%20Stone/1-01%20Everything%20Zen.mp3")
   (list "File Folder Count" 5)
   (list "Library Folder Count" 1)))

(define Bushtrack2
  (list
  (list "Track ID" 140)
  (list "Name" "Swim")
  (list "Artist" "Bush")
  (list "Album" "Sixteen Stone")
  (list "Genre" "Rock")
  (list "Kind" "MPEG audio file")
  (list "Size" 5949399)
  (list "Total Time" 295732)
  (list "Disc Number" 1)
  (list "Track Number" 2)
  (list "Year" 1994)
  (list "Date Modified" (create-date 2006 11 26 14 52 45))
  (list "Date Added" (create-date 2011 2 27 21 45 46))
  (list "Bit Rate" 160)
  (list "Sample Rate" 44100)
  (list "Play Count" 1)
  (list "Play Date" 3381673892)
  (list "Play Date UTC" (create-date 2011 2 27 21 51 32))
  (list "Artwork Count" 1)
  (list "Persistent ID" "146F561733A990A8")
  (list "Track Type" "File")
  (list
   "Location"
   "file://localhost/Users/z/Music/iTunes/iTunes%20Media/Music/Bush/Sixteen%20Stone/1-02%20Swim.mp3")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1)))

(define Nirvanatrack
  (list
  (list "Track ID" 114)
  (list "Name" "Smells Like Teen Spirit")
  (list "Artist" "Nirvana")
  (list "Composer" "Kurt Cobain, David Grohl, Chris Novoselic")
  (list "Album" "Nevermind")
  (list "Genre" "Rock")
  (list "Kind" "MPEG audio file")
  (list "Size" 7230247)
  (list "Total Time" 301165)
  (list "Track Number" 1)
  (list "Track Count" 12)
  (list "Year" 1991)
  (list "BPM" 192)
  (list "Date Modified" (create-date 2007 12 22 23 8 6))
  (list "Date Added" (create-date 2011 2 27 21 45 46))
  (list "Bit Rate" 192)
  (list "Sample Rate" 44100)
  (list "Play Count" 2)
  (list "Play Date" 3381674314)
  (list "Play Date UTC" (create-date 2011 2 27 21 58 34))
  (list "Persistent ID" "83B3927542025FDC")
  (list "Track Type" "File")
  (list
   "Location"
   "file://localhost/Users/z/Music/iTunes/iTunes%20Media/Music/Nirvana/Nevermind/01%20Smells%20Like%20Teen%20Spirit.mp3")
  (list "File Folder Count" 5)
  (list "Library Folder Count" 1)))

(define Nirvanatrackstruct
   (create-track
  "Smells Like Teen Spirit"
  "Nirvana"
  "Nevermind"
  301165
  1
  (create-date 2011 2 27 21 45 46)
  2
  (create-date 2011 2 27 21 58 34)))

; String, LAssoc, Any -> Assoc
; Returns the first association whose first item is equal to key, or default if there is no such Association
;(check-expect (find-association "Sample Rate" Nirvanatrack "N/A") (list "Sample Rate" 44100))
;(check-expect (find-association "Sample Sample" Nirvanatrack "N/A") "N/A")

(define (find-association key lassoc default)
  (cond [(empty? lassoc) default]
        [(cons? lassoc) (if (string=? key (first (first lassoc)))
                            (first lassoc)
                            (find-association key (rest lassoc) default))]))

; LList -> Number
; Consumes Llist, produces total amount of play-time
;(check-expect (total-time/list '()) 0)
;(check-expect (total-time/list (list Nirvanatrack)) (second (assoc "Total Time" Nirvanatrack)))
;(check-expect (total-time/list (list Nirvanatrack Girltalktrack1)) (+ (second (assoc "Total Time" Nirvanatrack)) (second (assoc "Total Time" Girltalktrack1))))
(define (total-time/list list)
  (cond [(empty? list) 0]
        [(cons? list) (+ (second (assoc "Total Time" (first list))) (total-time/list (rest list)))]))

; LList -> List of strings
; Consumes LList, produces a list-of-string for all boolean attribute names
;(check-expect (boolean-attributes (list Girltalktrack1)) (list "Part Of Gapless Album"))
;(check-expect (boolean-attributes (list Nirvanatrack)) '())

(define (boolean-attributes LList)
  (cond [(empty? LList) '()]
        [(cons? LList) (create-set (append (lassoc-boolean-attributes (first LList)) (boolean-attributes (rest LList))))]))

; Lassoc -> List of strings
; Consumes a LAssoc and returns a list of all boolean strings
;(check-expect (lassoc-boolean-attributes Girltalktrack1) (list "Part Of Gapless Album"))
(define (lassoc-boolean-attributes lassoc)
  (cond [(empty? lassoc) '()]
        [(cons? lassoc) (if (boolean? (second (first lassoc)))
                            (cons (first (first lassoc)) (lassoc-boolean-attributes (rest lassoc)))
                            (lassoc-boolean-attributes (rest lassoc)))]))

; List-of-strings -> List-of-strings
; Consumes a list of strings, and returns a list of strings that only contains one instance of each string in the original set
;(check-expect (create-set '()) '())
;(check-expect (create-set (list "hat")) (list "hat"))
;(check-expect (create-set (list "hat" "hat")) (list "hat"))
;(check-expect (create-set (list "hat" "man" "hat")) (list "man" "hat"))
(define (create-set l)
  (cond [(empty? l) '()]
        [(cons? l) (add-if-new (first l)
                               (create-set (rest l)))]))

; String -> List-of-strings
; Adds a string to a list-of-strings if the list does not already contain
; the string
(define (add-if-new s l)
  (if (member? s l)
      l
      (cons s l)))

; Lassoc -> Track
; Creates a track structure from an Lassoc to the extent possible
(check-expect (track-as-struct Nirvanatrack) Nirvanatrackstruct)
(define (track-as-struct lassoc)
  (create-track
   (second (assoc "Name" lassoc))
   (second (assoc "Artist" lassoc))
   (second (assoc "Album" lassoc))
   (second (assoc "Total Time" lassoc))
   (second (assoc "Track Number" lassoc))
   (second (assoc "Date Added" lassoc))
   (second (assoc "Play Count" lassoc))
   (second (assoc "Play Date UTC" lassoc))))
