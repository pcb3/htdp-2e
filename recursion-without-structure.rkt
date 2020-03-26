;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recursion-without-structure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 5 Generative Recursion

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

;;====
;; 421

; It produces (cons "" (bundle s n)) which is just the original
; function with the empty string consed onto the front.
; it is an infinite loop. It is improper because n = 0 is a
; condition that resets the function and starts another loop.

;;====
;; 422

; [List-of X] N -> [List-of [List-of X]]
; consumes a List l of arbitrary data and a Natural n and
; produces a list of lists of chunk size n where each chunk
; represents a sub-sequence of items in l
(check-expect (list->chunks '() 0) '())
(check-expect (list->chunks '("ab") 0) '())
(check-expect (list->chunks '("carrot" "daikon") 1)
              '(("carrot") ("daikon")))
(check-expect (list->chunks '("carrot" "daikon" "cabbage" "basil") 3)
              '(("carrot" "daikon" "cabbage") ("basil")))
(check-expect (list->chunks '("carrot" "daikon" "basil") 4)
              '(("carrot" "daikon" "basil")))

(define (fn-list->chunks l n)
  (cond
    [(empty? l) ...]
    [(zero? n) ...]
    [else
     (cons (take l n) (fn-list->chunks (drop l n) n))]))

(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else (cons (take l n) (list->chunks (drop l n) n))]))




























