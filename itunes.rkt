#!/usr/bin/racket
#lang htdp/bsl+

(require 2htdp/batch-io)
(require 2htdp/itunes)
(require test-engine/racket-tests)

; file-name of .xml library
(define f-tunes "lib.xml")

; LLists
; String -> LLists
; creates a list of lists representation for all tracks in 
; file-name, which must be an XML export from iTunes 
;
(define list-tracks (read-itunes-as-lists f-tunes))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)

(define LL1 '())

(define LL2 (cons
             (cons "foo" (cons "bar" '())) '()))
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)

(define LASSOC1 '())

(define LASSOC2 (cons
                 (cons "fizz" (cons #true '()))
                 (cons "buzz" (cons 42 '()))))

; An Association is a list of two items: 
;   (cons String (cons BSDN '()))

(define ASSOC1 (cons "fizz" (cons #true '())))

(define ASSOC2 (cons "buzz" (cons (create-date 2019 12 3 0 0 0) '())))

; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
 
; String LAssoc Any -> Assoc Default
; consumes a String key, a LAssoc las, and an element of Any default,
; and outputs the first Association whose first item is equal
; to key else default if no association 

(check-expect (find-association "Blueberry"
                                LASSOC1
                                "that key does not exist")
              "that key does not exist")

(check-expect (find-association "Name"
                                (list
  '("Track ID" 22655)
  '("Name" "Kids Return")
  '("Artist" "Jacques Greene 38 Cid Rim")
  '("Album Artist" "Jacques Greene 38 Cid Rim")
  '("Album" "Kids Return - Single")
  '("Genre" "Électronique")
  '("Kind" "Fichier audio AAC Apple Music")
  '("Size" 11023641)
  '("Total Time" 318000)
  '("Disc Number" 1)
  '("Disc Count" 1)
  '("Track Number" 1)
  '("Track Count" 1)
  '("Year" 2018)
  '("Bit Rate" 256)
  '("Sample Rate" 44100)
  (list "Release Date" (create-date 2018 12 17 8 0 0))
  '("Artwork Count" 1)
  '("Sort Album" "Kids Return - Single")
  '("Sort Artist" "Jacques Greene 38 Cid Rim")
  '("Sort Name" "Kids Return")
  '("Persistent ID" "99A9125087DE5966")
  '("Track Type" "Remote")
  '("Apple Music" #true)
  '("Playlist Only" #true)) 42)
              (list "Name" "Kids Return"))

(define (fn-find-association key las default)
  (cond
    [(empty? las) ...]
    [else (if (string=? key (... (first las)))
              (...)
              (fn-find-association key (rest las) default))]))

(define (find-association key las default)
  [cond
    [(empty? las) default]
    [else (if (string=? key (first (first las)))
              (first las)
              (find-association key (rest las) default))]])

; LLists -> Number
; consumes an LLists ll and outputs the total play time

(check-expect (total-time/list (list '())) 0)

(check-expect (total-time/list
                               (list (list
  '("Track ID" 22751)
  '("Name" "184: MDN Web Docs")
  '("Album" "The Web Platform Podcast")
  '("Genre" "Balado")
  '("Size" 79707353)
  '("Total Time" 3301000)
  (list "Date Added" (create-date 2019 3 2 8 11 58))
  (list "Release Date" (create-date 2019 2 26 7 42 15))
  '("Persistent ID" "8A4A55F09E3FF7D2")
  '("Clean" #true)
  '("Track Type" "URL")
  '("Podcast" #true)
  '("Unplayed" #true)
  '("Location"
    "http://traffic.libsyn.com/thewebplatform/WPP_184_MDN_Docs_mixdown.mp3?dest-id=209800"))
 (list
  '("Track ID" 22753)
  '("Name" "185: Houdini")
  '("Album" "The Web Platform Podcast")
  '("Genre" "Balado")
  '("Size" 79947095)
  '("Total Time" 3311000)
  (list "Date Added" (create-date 2019 3 2 8 11 58))
  (list "Release Date" (create-date 2019 2 28 18 2 35))
  '("Persistent ID" "2F6EB0D6CD085154")
  '("Clean" #true)
  '("Track Type" "URL")
  '("Podcast" #true)
  '("Unplayed" #true)
  '("Location"
    "http://traffic.libsyn.com/thewebplatform/WPP_185_Houdini_mixdown.mp3?dest-id=209800")))) 6612000)

(define (fn-total-time/list)
  (cond
    [(empty? ll) ...]
    [else (... (... (find-association "Total-time" (first ll) 0))
             (fn-total-time/list (rest ll)))]))

(define (total-time/list ll)
  (cond
    [(empty? ll) 0]
    [else (+ (second (find-association "Total-time" (first ll) 0)
                     (total-time/list (rest ll))))]))


(test)


