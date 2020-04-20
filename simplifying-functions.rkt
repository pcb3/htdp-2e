;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simplifying-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 19.6 Simplifying Functions

; Exercise 328

(define (atom? p)
  (or (number? p)
      (string? p)
      (symbol? p)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))


(check-expect (substitute '(((1 2 3) bye) bye) 'pee '42)
              '(((1 2 3) bye) bye))

(check-expect (substitute '() 9 10) '())

(define (substitute sexp old new)
  (cond
    [(atom? sexp)
     (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

; we need a lambda so we can iterate over sexp
; advancing each step and returning a new sexp






























