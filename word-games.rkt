#lang htdp/bsl+

; A Dictionary is a List-of-strings.
(define dictionary (read-lines "words"))

(require test-engine/racket-tests)

(require 2htdp/batch-io)

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)
 
; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))

; String -> Word
; converts s to the chosen word representation 

(check-expect (string->word '())
              '())

(check-expect (string->word "fizz")
              (cons "f" (cons "i" (cons "z" (cons "z" '())))))

(define (fn-string->word s)
  (cond
    [(empty? s) ...]
    [else (... (... (explode s)))]))

(define (string->word s)
  (cond
    [(empty? s) '()]
    [else (explode s)]))

; Word -> String
; converts w to a string

(check-expect (word->string '()) '())

(check-expect (word->string
               (cons "b" (cons "u" (cons "z" (cons "z" '())))))
              "buzz")

(define (fn-word-string w)
  (cond
    [(empty? w) ...]
    [else (... (...  (implode w)))]))

(define (word->string w)
  (cond
    [(empty? w) '()]
    [else (implode w)]))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 
; List-of-words -> List-of-strings
; turns all Words in low into Strings

(check-expect (words->strings '()) '())

(check-expect (words->strings
               (list
                (list "p" "i" "e")
                (list "l" "o" "v" "e")))
              (list "pie" "love"))

(define (fn-words->strings low)
  (cond
    [(empty? low) ...]
    [else (... (... (first low))
               (fn-words->strings (rest low)))]))

(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (implode (first low))
                  (words->strings (rest low)))]))
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary

(check-expect (in-dictionary '()) '())

(check-expect (in-dictionary
               (list "I" "love" "pklashdf"))
              (list "I" "love"))

(define (fn-in-dictionary los)
  (cond
    [(empty? los) ...]
    [else (if (... (first los))
              (... (first los)
                   (fn-in-dictionary (rest los)))
              (fn-in-dictionary (rest los)))]))

(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [else (if (match? (first los) dictionary)
              (cons (first los)
                    (in-dictionary (rest los)))
              (in-dictionary (rest los)))]))

; String Dictionary -> Boolean
; consumes a string s and a dictionary d and outputs
; true if the string exists in the dictionary

(check-expect (match? "" (list "pew" "die" "pie")) #false)

(check-expect (match? "pew" (list "pew" "die" "pie")) #true)

(define (fn-match? s d)
  (cond
    [(empty? d) ...]
    [else (if (... (... s (first d)))
              #true
              (fn-match? s (rest d)))]))

(define (match? s d)
  (cond
    [(empty? d) #false]
    [else (if (string=? s (first d))
              #true
              (match? s (rest d)))]))

(test)
