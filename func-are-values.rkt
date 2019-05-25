;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname func-are-values) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 14.2 Functions are values

;; exercise 245

; Develop the function=at-1.2-3-and-5.775? function.
; Given two functions from numbers to numbers, the function determines
; whether the two produce the same results for 1.2, 3, and -5.775.

; Function Function -> Boolean
; consumes two functions f1 nd f2, and returns true if they
; have equal output for 1.2, 3 and -5.775

(check-expect (function=? + +) #true)

(check-expect (function=? + -) #false)

(define (fn-function=? f1 f2)
  (cond
    [(and (equal? (f1 1.2) (f2 1.2))
          (equal? (f1 3) (f2 3))
          (equal? (f1 -5.775) (f2 -5.775)))
     ...]
    [else ...]))

(define (function=? f1 f2)
  (cond
    [(and (equal? (f1 1.2) (f2 1.2))
          (equal? (f1 3) (f2 3))
          (equal? (f1 -5.775) (f2 -5.775)))
     #true]
    [else #false]))