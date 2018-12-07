;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Chapter 13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; the 2htdp/itunes library documentation, part 1: 
 
; An LTracks is one of:
; – '()
; – (cons Track LTracks)
 

; modify the following to use your chosen name
(define ITUNES-LOCATION "iTunes Music Library.xml")


(define Bushtrack
  (create-track
   "Comedown"
   "Bush"
   "Sixteen Stone"
   326739
   5
   (create-date 2011 2 27 21 45 46)
   1
   (create-date 2011 2 27 21 51 41)))

(define Girltalktrack
  (create-track
   "Play Your Part (Pt. 1)"
   "Girl Talk"
   "Feed The Animals"
   284865
   1
   (create-date 2011 2 27 21 45 44)
   3
   (create-date 2011 2 27 21 52 6)))

(define Nirvanatrack
   (create-track
  "Smells Like Teen Spirit"
  "Nirvana"
  "Nevermind"
  301165
  1
  (create-date 2011 2 27 21 45 46)
  2
  (create-date 2011 2 27 21 58 34)))

(define Bush2track
  (create-track
   "X-Girlfriend"
   "Bush"
   "Sixteen Stone"
   45139
   12
   (create-date 2011 2 27 21 45 47)
   1
   (create-date 2011 2 27 21 52 3)))

(define Grungelibrary
  (list Bushtrack Nirvanatrack Bush2track))

 
; LTracks
(define mylibrary
  (read-itunes-as-tracks ITUNES-LOCATION))

; LTracks->Number
; Consumes an Ltracks library, returns total play time
;(check-expect (total-time (list Bushtrack)) (* (track-time Bushtrack) (track-play# Bushtrack)))
;(check-expect (total-time (list Bushtrack Bush2track)) (+ (* (track-time Bushtrack) (track-play# Bushtrack)) (* (track-time Bush2track) (track-play# Bush2track))))
(define (total-time ltracks)
  (cond [(empty? ltracks) 0]
        [(cons? ltracks) (+ (total-track-playtime (first ltracks)) (total-time (rest ltracks)))]))

; Track->Number
; Consumes a track, calculates total play time
;(check-expect (total-track-playtime Bushtrack) (* (track-time Bushtrack) (track-play# Bushtrack)))
(define (total-track-playtime track)
  (* (track-time track) (track-play# track)))

; LTracks -> List-of-strings
; Consumes an Ltracks, returns a list of strings containing all library albums
;(check-expect (select-all-album-titles '()) '())
;(check-expect (select-all-album-titles Grungelibrary) (list (track-album Bushtrack) (track-album Nirvanatrack) (track-album Bush2track)))
(define (select-all-album-titles library)
  (cond [(empty? library) '()]
        [(cons? library) (cons (track-album (first library)) (select-all-album-titles (rest library)))]))

; LTracks -> List-of-strings
; Consumes an Ltracks, returns a list of unique strings for albums of tracks contained in the library of tracks
;(check-expect (select-album-titles/unique (list Bushtrack Bush2track)) (list (track-album Bushtrack)))
(define (select-album-titles/unique library)
  (create-set (select-all-album-titles library)))

; String, Date, Ltracks -> Ltracks
; Returns a list of all tracks with the album of the given string, played after the given date
;(check-expect (select-album-date "Sixteen Stone" (create-date 2011 2 26 21 45 46) '()) '())
;(check-expect (select-album-date "Sixteen Stone" (create-date 2011 2 26 21 45 46) Grungelibrary) (list Bushtrack Bush2track))
;(check-expect (select-album-date "Sixteen Stone" (create-date 2018 2 26 21 45 46) Grungelibrary) '())

(define (select-album-date a d library)
  (cond [(empty? library) '()]
        [(cons? library) (if (and (check-track-for-album a (first library))
                                  (check-track-for-date d (first library)))
                             (cons (first library) (select-album-date a d (rest library)))
                             (select-album-date a d (rest library)))]))

; String, Track -> Boolean
; Checks whether a track has the album track for the given string, returns true is so, else false
;(check-expect (check-track-for-album "Sixteen Stone" Bushtrack) #true)
;(check-expect (check-track-for-album "Sixteen Stone" Girltalktrack) #false)
(define (check-track-for-album a trk)
  (string=? a (track-album trk)))

; Date, Track -> Boolean
; Checks whether a track has a date after the given date, and returns #true if so, else #false
;(check-expect (check-track-for-date (create-date 2011 2 26 21 45 46) Bushtrack) #true)
;(check-expect (check-track-for-date (create-date 2018 1 1 1 1 1) Bushtrack) #false)
(define (check-track-for-date d trk)
  (later-date? (track-played trk) d))

; Date Date -> Boolean
; Returns #true if first Date after second Date
;(check-expect (later-date? (create-date 2000 1 1 1 1 1) (create-date 2000 1 1 1 1 1)) #true)
;(check-expect (later-date? (create-date 2000 1 1 1 1 1) (create-date 2000 1 1 1 1 2)) #false)
(define (later-date? d1 d2)
  (or (> (date-year d1) (date-year d2))
      (and (= (date-year d1) (date-year d2)) (< (date-month d1) (date-month d2)))
      (and (= (date-year d1) (date-year d2)) (= (date-month d1) (date-month d2)) (> (date-day d1) (date-day d2)))
      (and (= (date-year d1) (date-year d2)) (= (date-month d1) (date-month d2) (= (date-day d1) (date-day d2))) (> (date-hour d1) (date-hour d2)))
       (and (= (date-year d1) (date-year d2)) (= (date-month d1) (date-month d2) (= (date-day d1) (date-day d2))) (= (date-hour d1) (date-hour d2)) (> (date-minute d1) (date-minute d2)))
       (and (= (date-year d1) (date-year d2)) (= (date-month d1) (date-month d2) (= (date-day d1) (date-day d2))) (= (date-hour d1) (date-hour d2)) (= (date-minute d1) (date-minute d2)) (> (date-second d1) (date-second d2)))))

; Ltracks -> List of Ltracks
; Consumes an Ltrack, returns a list of Ltracks, with each consisting of a unique album Ltrack including all tracks listed with that album
;(check-expect (select-albums (list Bushtrack Nirvanatrack Bush2track Girltalktrack)) (list (list Nirvanatrack) (list Bushtrack Bush2track) (list Girltalktrack)))
(define (select-albums library)
  (cond [(empty? library) '()]
        [(cons? library) (divide-library-by-albums
        (select-album-titles/unique library)
        library)]))

; List-of-strings, Ltracks -> List of Ltracks
; Divides a library of Ltracks into separate lists for each album with each list consisting of all tracks for that album
;(check-expect (divide-library-by-albums (list (track-album Bushtrack) (track-album Nirvanatrack) (track-album Girltalktrack)) (list Bushtrack Nirvanatrack Bush2track Girltalktrack)) (list (list Bushtrack Bush2track) (list Nirvanatrack) (list Girltalktrack)))
(define (divide-library-by-albums album-list library)
  (cond [(empty? album-list) '()]
        [(cons? album-list) (cons (select-all-album-tracks (first album-list) library) (divide-library-by-albums (rest album-list) (remove-all-album-tracks (first album-list) library)))]))

; String, LTracks -> LTracks
; Returns a list of all tracks with String as album
;(check-expect (select-all-album-tracks (track-album Bushtrack) (list Bushtrack Nirvanatrack Bush2track Girltalktrack)) (list Bushtrack Bush2track))
(define (select-all-album-tracks album library)
  (cond [(empty? library) '()]
        [(cons? library) (if (check-track-for-album album (first library))
                             (cons (first library) (select-all-album-tracks album (rest library)))
                             (select-all-album-tracks album (rest library)))]))

; String, LTracks -> LTracks
; Returns a list of all tracks with albums that are not String
;(check-expect (remove-all-album-tracks (track-album Bushtrack) (list Bushtrack Nirvanatrack Bush2track Girltalktrack)) (list Nirvanatrack Girltalktrack))
(define (remove-all-album-tracks album library)
  (cond [(empty? library) '()]
        [(cons? library) (if (not (check-track-for-album album (first library)))
                             (cons (first library) (remove-all-album-tracks album (rest library)))
                               (remove-all-album-tracks album (rest library)))]))

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


; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

