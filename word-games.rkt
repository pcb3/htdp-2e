#lang htdp/bsl+

(require test-engine/racket-tests)

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

(test)
