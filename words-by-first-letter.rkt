;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname words-by-first-letter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/batch-io)

; A Dictionary is a List-of-strings
(define DICT (read-lines "words.txt"))

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define DICT1 (list "apple" "avocado" "banana"
                    "blueberry" "boysenberry"
                    "cherry" "clamentine"))

(define lolod1 (list
                (list "apple")
                (list "banana" "blueberry" "boysenberry")
                (list "cherry" "clamentine")))

(define-struct Letter-Counts [letter count])
; a letter-counts is a structure
; (make-frequency Letter Number)
; interpretation: a tally of the number of times each
; letter in Letter appears in a Dictionary

(define LC1 (make-Letter-Counts "e" 14197))
(define LC2 (make-Letter-Counts "z" 1388))


; ex 198 words-by-first-letter

; Design words-by-first-letter. The function consumes a Dictionary
; and produces a list of Dictionarys, one per Letter.

; a List-of-Dictionaries is one of:
; - (list Dictionary)
; - (cons Dictionary List-of-Dictionaries)

; Dictionary -> LoD
; consumes a Dictionary dict and outputs a list of dictionaries,
; one per letter

(check-expect (words-by-first-letter DICT1)
              (list (list "apple" "avocado")
                    (list "banana" "blueberry" "boysenberry")
                    (list "cherry" "clamentine")))

(define (fn-words-by-first-letter dict)
  (cond
    [(empty? dict) ...]
    [else (... (... (first dict))
               (... (fn-words-by-letter (rest dict))))]))

(define (words-by-first-letter dict)
  (cond
    [(empty? (rest dict)) (cons dict '())]
    [else (if (compare-letter (first dict) (second dict))
              (insert (first dict) (words-by-first-letter (rest dict)))
              [cons (list (first dict)) (words-by-first-letter (rest dict))])]))
              
; String String -> Boolean
; if first letter of word and first letter are equal, return true
(define (compare-letter word1 word2)
  (cond
    [(boolean=? #true (string=? (first (explode word1))
                                (first (explode word2))))
     #true]
    [else #false]))

; String List-of-List-of-Strings -> List-of-List-of-Strings (lolos)
; inserts a String s into the first list in a list of list of strings

(check-expect (insert "apple" (list (list "aperture")
                                    (list "banana")))
              (list (list "apple" "aperture")
                    (list "banana")))

(define (insert str lolos)
  (cons (cons str (first lolos))
        (rest lolos)))

; redesign most-frequent from ex 197

; Design most-frequent. The function consumes a Dictionary.
; It produces the Letter-Count for the letter that occurs most often
; as the first one in the given Dictionary.

; Dictionary -> Letter-Count
; consumes a dictionary and outputs the most frequent letter count pair

(check-expect (most-frequent2 DICT1)
              (make-Letter-Counts "b" 3))

(define (fn-most-frequent2 dict)
  (... (... (dict))))

(define (most-frequent2 dict)
  [make-Letter-Counts
   (first (explode (first (first (sort-by-length (words-by-first-letter dict))))))
   (length (first (sort-by-length (words-by-first-letter dict))))])

; List-of-List-of-Dictionaries -> List-of-List-of-Dictionaries
; consumes a LoLod and outputs a sorted LoLoD by length
; in decreasing order

(check-expect (sort-by-length lolod1)
              (list (list "banana" "blueberry" "boysenberry")
                    (list "cherry" "clamentine")
                    (list "apple")))

(define (fn-sort-by-length lolod)
  (cond
    [(empty? lolod) ...]
    [else (... (... (first lolod))
               (... (fn-sort-by-length (rest lolod))))]))

(define (sort-by-length lolod)
  (cond
    [(empty? lolod) '()]
    [else (insert-dict (first lolod)
                       (sort-by-length (rest lolod)))]))

(define (insert-dict n dict)
  (cond
    [(empty? dict) (list n)]
    [else (if (>= (length n)
                  (length (first dict)))
              (cons n dict)
              (cons (first dict) (insert-dict n (rest dict))))]))









































