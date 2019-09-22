;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname specifying-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 17.4 Specifying with lambda

; exercise 292

; [X X -> Boolean] [NEList-of X] -> Booelan
; determines whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (fn-sorted? cmp l)
  (cond
    [(empty? (rest l)) ...]
    [else (if (cmp (... l) (... (... l)))
              (fn-sorted? cmp (rest l))
              ...)]))

(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [else (if (cmp (first l) (first (rest l)))
              (sorted? cmp (rest l))
              #false)]))
  