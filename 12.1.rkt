;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 12 Project: Lists
; 12.1 Real-World Data: Dictionaries

(require 2htdp/batch-io)

; A Dictionary is a List-of-strings
(define DICT (read-lines "words.txt"))

(define DICT0 '()) ; the empty dictionary
(define DICT1 (list "apple" "banana" "cape"))
(define DICT2
  (list "apple" "bat" "berillium"
        "cape" "capitalism" "communism"))
(define DICT3
  (list "apple" "avocado" "banana"
        "blueberry" "boysenberry"
        "cherry" "clamentine"))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; ex 195

; Letter Dict -> Number
; consumes a Letter let and a Dictionary dict and
; outputs the number of occurences of
; said letter in the dictionary

(check-expect (starts-with# "a" DICT0) 0)
(check-expect (starts-with# "b" DICT1) 1)
(check-expect (starts-with# "c" DICT2) 3)

(define (fn-starts-with# let dict)
  (cond
    [(empty? dict) ...]
    [(string=? let
               (... (first dict) ...))
     (... (fn-starts-with# let (rest dict)))]
    [else (fn-starts-with# let (rest dict))]))

(define (starts-with# let dict)
  (cond
    [(empty? dict) 0]
    [(string=? let (first (explode (first dict))))
     (add1 (starts-with# let (rest dict)))]
    [else (starts-with# let (rest dict))]))


; (starts-with# "e" DICT) ; 14197 words start with "e"
; (starts-with# "z" DICT) ; 1388 words start with "z"

; ex 196 

(define-struct Letter-Counts [letter count])
; a letter-counts is a structure
; (make-frequency Letter Number)
; interpretation: a tally of the number of times each
; letter in Letter appears in a Dictionary

(define LC1 (make-Letter-Counts "e" 14197))
(define LC2 (make-Letter-Counts "z" 1388))

; a List-of-Letter-Counts (LoLC) is one of:
; - (list Letter-Counts)
; - (cons Letter-Counts List-of-Letter-Counts)

(define LOF1 (list (make-Letter-Counts "e" 14197)))

(define LOF2 (list (make-Letter-Counts "e" 14197)
                   (make-Letter-Counts "z" 1388)))

; Dictionary -> LoLC
; consumes a Dictionary dict and
; outputs a LoLC containing the frequency
; of each letter count pair

(check-expect (count-by-letter LETTERS DICT1)
              (list
               (make-Letter-Counts "a" 1)
               (make-Letter-Counts "b" 1)
               (make-Letter-Counts "c" 1)
               (make-Letter-Counts "d" 0)
               (make-Letter-Counts "e" 0)
               (make-Letter-Counts "f" 0)
               (make-Letter-Counts "g" 0)
               (make-Letter-Counts "h" 0)
               (make-Letter-Counts "i" 0)
               (make-Letter-Counts "j" 0)
               (make-Letter-Counts "k" 0)
               (make-Letter-Counts "l" 0)
               (make-Letter-Counts "m" 0)
               (make-Letter-Counts "n" 0)
               (make-Letter-Counts "o" 0)
               (make-Letter-Counts "p" 0)
               (make-Letter-Counts "q" 0)
               (make-Letter-Counts "r" 0)
               (make-Letter-Counts "s" 0)
               (make-Letter-Counts "t" 0)
               (make-Letter-Counts "u" 0)
               (make-Letter-Counts "v" 0)
               (make-Letter-Counts "w" 0)
               (make-Letter-Counts "x" 0)
               (make-Letter-Counts "y" 0)
               (make-Letter-Counts "z" 0)))

(define (fn-count-by-letter lol dict)
  (cond
    [(empty? dict) ...]
    [else (... (... (Letter-Counts-letter lol)
                    (Letter-Counts-counts lol))
               (fn-count-by-letter (rest lol) dict))]))

(define (count-by-letter lol dict)
  (cond
    [(empty? lol) '()]
    [else (cons (make-Letter-Counts
                 (first lol)
                 (starts-with# (first lol) dict))
                (count-by-letter (rest lol) dict))]))

; ex 197 design most-frequent

; design a function that picks the pair with the maximum count

; Dictionary -> Letter-Count
; consumes a dictionary and outputs the Letter-Counts for
; the letter with the highest occurences in the given dictionary

(check-expect (most-frequent DICT)
              (make-Letter-Counts "s" 10019))
                          
(define (most-frequent dict)
  (pick-max-count (count-by-letter LETTERS dict)))

; LoLC -> Letter-Counts
; consumes a List of Letter-Counts lolc, compares two pairs of
; Letter-Counts and outputs the pair with max count

(check-expect (pick-max-count (list (make-Letter-Counts "a" 0)
                                    (make-Letter-Counts "b" 10)
                                    (make-Letter-Counts "c" 5)))
              (make-Letter-Counts "b" 10))

(define (pick-max-count lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (extract-compare-count (first lolc)
                                 (pick-max-count (rest lolc)))]))

(define (extract-compare-count LC1 LC2)
  (if (>= (Letter-Counts-count LC1)
          (Letter-Counts-count LC2))
      LC1
      LC2))

; tested max of the max function instead of composition of 2,
; didnt produce desired result

(define (just-use-max lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (max (Letter-Counts-count (first lolc))
               (Letter-Counts-count (just-use-max (rest lolc))))]))

; design a function that selects the first from a sorted list of pairs

; Dictionary -> Letter-Counts
; consumes a dictionary and outputs the first Letter-Count in a
; sorted list of Letter-Counts

(check-expect (most-frequent-sorted DICT)
              (make-Letter-Counts "s" 10019))

(define (fn-most-frequent-sorted dict)
  (... (count-by-letter LETTERS dict)))

(define (most-frequent-sorted dict)
  (first (sort-count (count-by-letter LETTERS dict))))

; LoLC -> LoLC
; consumes a list of Letter-Counts and outputs a sorted list in
; descending order by count

(define (fn-sort-count lolc)
  (cond
    [(empty lolc) '()]
    [else (... (first lolc)
               (fn-sort-count (rest lolc)))]))

(define (sort-count lolc)
  (cond
    [(empty? lolc) '()]
    [else (insert (first lolc)
                  (sort-count (rest lolc)))]))

(define (insert n lolc)
  (cond
    [(empty? lolc) (list n)]
    [else (if (>= (Letter-Counts-count n) (Letter-Counts-count (first lolc)))
              (cons n lolc)
              (cons (first lolc) (insert n (rest lolc))))]))

; which method do you prefer, taking the max of two pairs of Letter-Counts or,
; sorting the List first and then taking the first item

; I prefer the sorted method. It keeps things simple and uses previously
; defined or built-in functions like sort and insert.
; also the code was easier to reason about. The first method was very complex
; at first aand more difficult to tease out the individual functions

; ex 198
; Design words-by-first-letter. The function consumes a Dictionary
; and produces a list of Dictionarys, one per Letter.

; a List-of-Dictionaries is one of:
; - (list Dictionary)
; - (cons Dictionary List-of-Dictionaries)

; Dictionary -> LoD
; consumes a Dictionary dict and outputs a list of dictionaries,
; one per letter

;(check-expect (words-by-first-letter (list "apple" "banana" "cantelope"))
;              (list (list "apple")
;                    (list "banana")
;                    (list "cantelope")))

(define (fn-words-by-first-letter dict)
  (cond
    [(empty? dict) ...]
    [else (... (... (first dict))
               (... (fn-words-by-letter (rest dict))))]))

(define (words-by-first-letter dict)
  (cond
    [(empty? dict) '()]
    [else (cons (dict-by-letter dict LETTERS)
                (words-by-first-letter (rest dict)))]))

; Dictionary Letter -> List-of-Strings
; consumes a dictionary dict and a Letter l, creates a new dictionary and
; adds all words that share the same first letter to it

;(check-expect (dict-by-letter (list "apple" "banana" "cantelope") LETTERS)
;              (list "apple"))

(define (fn-dict-by-letter dict l)
  (cond
    [(empty? dict) ...]
    [else (if (string=? (first dict) (first l))
              (... (first dict) (fn-dict-by-letter (rest dict) l))
              (fn-dict-by-letter (first dict) (rest l)))]))           

(define (dict-by-letter dict l)
  (cond
    [(empty? dict) '()]
    [else (if (string=? (first (explode (first dict))) (first l))
              (cons (first dict) (dict-by-letter (rest dict) l))
              (dict-by-letter (first dict) (rest l)))]))

    



























