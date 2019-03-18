#lang htdp/bsl

; A Dictionary is a List-of-strings.
(define dictionary (read-lines "words"))

(require test-engine/racket-tests)

(require 2htdp/batch-io)

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

(define WORD1 '())
(define WORD2 (list "p" "e" "w"))
(define WORD3 (list "d" "i" "e"))

; A List-of-Words is one of:
; - '()
; - (cons Word List-of-words)
; interpretation a List-of-Words is a list of a list of 1Strings
; or the empty list

(define LOW1 '())
(define LOW2 (list
              (list "p" "e" "w")
              (list "d" "i" "e")
              (list "p" "i" "e")))
(define LOW3 (list
              (list "f" "r" "e" "d")
              (list "h" "a" "r" "r" "y")
              (list "j" "u" "l" "i" "a")))
 
; Word -> List-of-words
; finds all rearrangements of word

(check-expect (arrangements '()) (list '()))

(check-expect (arrangements (list "e" "d"))
              (cons (list "e" "d")
                    (cons  (list "d" "e")
                           '())))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (fn-arrangements w)
  (cond
    [(empty? w) ...]
    [else (... (first w) ...
           ... (fn-arrangements (rest w)) ...)]))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; 1String List-of-Words -> List-of-Words
; conusmes a 1String s and inserts it at the beginning, end,
; and in-between all letters of one arrangement of list of words 

(check-expect (insert-everywhere/in-all-words "d" '())
              (cons (list "d") '()))

(check-expect
 (insert-everywhere/in-all-words "d"
                                 (cons  (list "p") '()))
 (cons (list "d" "p")
       (cons (list "p" "d")
             '())))

(check-expect
 (insert-everywhere/in-all-words "d"
                                 (cons (list "p")
                                       (cons (list "i")
                                             '())))
 (cons (list "d" "p" "i")
       (cons (list "d" "i" "p")
             (cons (list "p" "d" "i")
                   (cons (list "p" "i" "d")
                         (cons (list "i" "p" "d")
                               (cons (list "i" "d" "p")
                                     '())))))))

(define (fn-insert-everywhere/in-all-words s w)
  (cond
    [(empty? w) ...]
    [else (... (... s (first w))
               (fn-insert-everywhere/in-all-words s (rest w)))]))

(define (insert-everywhere/in-all-words s w)
  (cond
    [(empty? w) (list '())]
    [else (cons (append (list s) (first w))
                (insert-everywhere/in-all-words s (rest w)))]))

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
