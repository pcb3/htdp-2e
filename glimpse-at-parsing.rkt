;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname glimpse-at-parsing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; File -> [List-of Line]
; converts a file into a list of lines 
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
     (cons (first-line afile)
           (file->list-of-lines (remove-first-line afile)))]))
             
; File -> Line
; consumes a file and produces a line at "\n" or at the end of
; the file
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) '()]
    [else (cons (first afile) (first-line (rest afile)))]))
             
; File -> Line
; consumes a file and removes the the first line and returns
; the rest of the file
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) NEWLINE) (rest afile)]
    [else (remove-first-line (rest afile))]))
             
(define NEWLINE "\n") ; the 1String

;;====
;; 453

; A Token is one of:
; - 1String or
; - String
; A Token is either a 1String of non-letters or a String of
; letters. There is no whitespace

; Line -> [List-of Token]
; consumes a a Line line and produces a list of tokens.
; all white-space 1Strings are dropped,non-letter 1Strings
; are cons'ed onto letter 1Strings that are bundled together

(check-expect (tokenize '()) '())
(check-expect (tokenize '("1")) '("1"))
(check-expect (tokenize '("a" "b" "c")) '("abc"))
(check-expect (tokenize '(" " "a" " ")) '("a"))

(define (fn-tokenize line)
  (cond
    [(empty? line) ...]
    [(string-numeric? (first line))
     (...(first line) (fn-tokenize (rest line)))]
    [else (cons (bundle line)
                (fn-tokenize (remove-string line)))]))

(define (fn-bundle l)
  (cond
    [(empty? l) ...]
    [else (... (first l) (fn-bundle (rest l)))]))

(define (fn-remove-string l)
  (cond
    [(empty? l) ....]
    [(or (string-numeric? (first l))
         (string-whitespace? (first l))) (rest l)]
    [else (fn-remove-string (rest l))]))
     
(define (tokenize line)
  (cond
    [(empty? line) '()]
    [(string-numeric? (first line))
     (cons (first line) (tokenize (rest line)))]
    [(string-whitespace? (first line))
     (tokenize (rest line))]
    [else (cons (implode (bundle line))
                (tokenize (remove-string line)))]))

(define (bundle l)
  (cond
    [(empty? l) '()]
    [(red-or-white-queen? (first l)) (rest l)]
    [else (cons (first l) (bundle (rest l)))]))

(define (remove-string l)
  (cond
    [(empty? l) '()]
    [(red-or-white-queen? (first l)) (rest l)]
    [else (remove-string (rest l))]))

(define (red-or-white-queen? 1str)
  (or (string-numeric? 1str)
      (string-whitespace? 1str)))

;;====
;; 454

; A Matrix is a:
; - '()
; - (cons (list Number) Matrix)

; a Matrix is either the empty matrix '() or a n x n list of
; lists of numbers

; Number [List-of Number] -> Matrix
; consumes a number n and a list of n squared numbers
; n-squared, and produces a n x n matrix

(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))
(check-expect (create-matrix 1 (list 1)) (list (list 1)))
(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))

(define (fn-create-matrix n n-squared)
  (cond
    [(empty? n-squared) ...]
    [else
     (... (take-n n n-squared)
          (fn-create-matrix ... (remove-n n n-squared)))]))

(define (create-matrix n n-squared)
  (cond
    [(empty? n-squared) '()]
    [else
     (cons (take-n n n-squared)
           (create-matrix n (remove-n n n-squared)))]))

(define (take-n n n-squared)
  (cond
    [(zero? n) '()]
    [else
     (cons
      (first n-squared)
      (take-n (sub1 n) (rest n-squared)))]))

(define (remove-n n n-squared)
  (cond
    [(zero? n) n-squared]
    [else
     (remove-n (sub1 n) (rest n-squared))]))
     























