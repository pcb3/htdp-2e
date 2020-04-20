;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |12.2 iTunes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)
(require 2htdp/batch-io)
(require racket/date)

; modify the following to use your chosen name
(define ITUNES-LOCATION "Library.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; Freddy's itunes library
(define f-tunes
  (read-itunes-as-tracks "lib.xml"))

(define (select-all-albums lt)
  (cond
    [(empty? lt) '()]
    [else (cons (track-album (first lt))
                (select-all-albums (rest lt)))]))
    
(define (create-set los)
  (cond
    [(empty? los) '()]
    [else (if (is-member? (first los) (rest los))
              (create-set (rest los))
              (cons (first los) (create-set (rest los))))]))

(define (is-member? s los)
  (cond
    [(empty? los) #false]
    (else (if (equal? s (first los))
              #true
              (is-member? s (rest los))))))

(create-set
 (list "apple"
       "banana"
       "cantelope"
       "banana"
       "apple"
       "banana"
       "cantelope"
       "banana"))

;(define (select-album-titles/unique lt)
;  (create-set (select-all-albums itunes-tracks)))

;(length (create-set (select-all-albums
;             (read-itunes-as-tracks "lib.xml"))))

; String LTracks -> List-of-Strings
; consumes an album title al and an LTrack lt and
; outputs a list of tracks that belong to the given album

(check-expect (select-album "" '()) '())

(define (fn-select-album al lt)
  (cond
    [(empty? lt) ...]
    [else (if (string=? al (track-album (first lt)))
              (... (track-name (first lt))
                   (fn-select-album al (rest lt)))
              (fn-select-album al (rest lt)))]))

(define (select-album al lt)
  (cond
    [(empty? lt) '()]
    [else (if (string=? al (track-album (first lt)))
              (cons (track-name (first lt))
                    (select-album al (rest lt)))
              (select-album al (rest lt)))]))

; A list-of-Tracks is one of:
; - '()
; - (cons Track List-of-Tracks)

; String Date LTracks -> List-of-Tracks
; extracts a list of Tracks (lot) that belong to the given
; album (al) that have been played after a given date (dt)
; in the collection of LTracks (lt)

(check-expect
 (select-album-date "T R A P S O U L"
                    (create-date 2018 12 18 0 0 0)
                    '())
 '())

(check-expect
 (select-album-date "T R A P S O U L"
                    (create-date 2018 12 8 18 6 0)
                    itunes-tracks)
 (list 
  (create-track
   "Been That Way"
   "Bryson Tiller"
   "T R A P S O U L"
   199392
   12
   (create-date 2018 12 8 3 40 53)
   3
   (create-date 2018 12 8 18 6 6))
  (create-track
   "Overtime"
   "Bryson Tiller"
   "T R A P S O U L"
   218328
   13
   (create-date 2018 12 8 3 40 53)
   3
   (create-date 2018 12 8 18 9 44))
  (create-track
   "Right My Wrongs"
   "Bryson Tiller"
   "T R A P S O U L"
   249384
   14
   (create-date 2018 12 8 3 40 53)
   3
   (create-date 2018 12 8 18 13 53))))

(define (fn-select-album-date al dt lt)
  (cond
    [(empty? lt) ...]
    [else (if (and (string=? al (track-album (first lt)))
                   (date< dt (first lt)))
              (... (first lt)
                   (fn-select-album-date al dt (rest lt)))
              (fn-select-album-date al dt (rest lt)))]))

(define (select-album-date al dt lt)
  (cond
    [(empty? lt) '()]
    [else (if (and (string=? al (track-album (first lt)))
                   (date< dt (track-played (first lt))))
              (cons (first lt)
                    (select-album-date al dt (rest lt)))
              (select-album-date al dt (rest lt)))]))

; Track Date -> Boolean
; compares dates of a given Track played
; with a given Date added, returns true if played is later
; in time than added

(check-expect (date< (create-date 2018 1 1 1 1 1)
                     (create-date 2018 2 1 1 1 1)) #t)

(check-expect (date< (create-date 2018 1 1 1 1 1)
                     (create-date 2019 1 1 1 1 1)) #t)

(define (date< added played)
  (cond
    [(< (date-year added) (date-year played)) #true]
    [(> (date-year added) (date-year played)) #false]
    [(< (date-month added) (date-month played)) #true]
    [(> (date-month added) (date-month played)) #false]
    [(< (date-day added) (date-day played)) #true]
    [(> (date-day added) (date-day played)) #false]
    [(< (date-hour added) (date-hour played)) #true]
    [(> (date-hour added) (date-hour played)) #false]
    [(< (date-minute added) (date-minute played)) #true]
    [(> (date-minute added) (date-minute played)) #false]
    [(< (date-second added) (date-second played)) #true]
    [else #false]))

; a list-of-LTracks (LLT) is one of:
; - (list '())
; - (cons LTrack List-of-LTracks)

; LTracks -> List-of-LTracks
; consumes an Ltracks and creates a List-of-LTracks,
; one per album

;(check-expect (select-albums '()) (list '()))

;(check-expect (select-albums
;              (list
;               (make-track
;  "Right My Wrongs"
;  "Bryson Tiller"
;  "T R A P S O U L"
;  249384
;  14
;  (make-date 2018 12 8 3 40 53)
;  3
;  (make-date 2018 12 8 18 13 53))
; (make-track
;  "01 Easy Lee"
;  "Villalobos"
;  "Alcachofa"
;  606563
;  1
;  (make-date 2018 12 8 4 29 52)
;  1
;  (make-date 2018 12 8 4 40 10))
; (make-track
;  "05 Waiworinao"
;  "Villalobos"
;  "Alcachofa"
;  490501
;  5
;  (make-date 2018 12 8 4 29 52)
;  1
;  (make-date 2018 12 8 4 52 1))))
;(list
; (list
;  (make-track
;  "Right My Wrongs"
;  "Bryson Tiller"
;  "T R A P S O U L"
;  249384
;  14
;  (make-date 2018 12 8 3 40 53)
;  3
;  (make-date 2018 12 8 18 13 53)))
; (list
;  (make-track
;  "01 Easy Lee"
;  "Villalobos"
;  "Alcachofa"
;  606563
;  1
;  (make-date 2018 12 8 4 29 52)
;  1
;  (make-date 2018 12 8 4 40 10))
; (make-track
;  "05 Waiworinao"
;  "Villalobos"
;  "Alcachofa"
;  490501
;  5
;  (make-date 2018 12 8 4 29 52)
;  1
;  (make-date 2018 12 8 4 52 1)))))

(define (list-of-albums lt)
   (select-album (first (set-of-albums lt)) lt)) 

; LTracks -> LTracks
; consumes an LTracks lt and outputs an LTracks containing
; a set of unique albums
(define (set-of-albums lt)
  (create-set (select-all-albums lt)))

(define (grab-tracks-by-album lt loa)
  (cond
    [(empty? loa) '()]
    [else (cons (select-album (first loa) lt)
                (grab-tracks-by-album lt (rest loa)))]))

(define album-set
  (set-of-albums f-tunes))














































