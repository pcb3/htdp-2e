;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname existing-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 16.1 Existing abstractions

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-l*st n f) == (list (f 0) ... (f (- n 1)))

(check-expect (build-l*st 3 add1) (list 1 2 3))

(define (fn-build-l*st n f)
  (... f (list-0-n-1 n b)))

(define (build-l*st n f)
  (map f (list-0-n-1 n 0)))

(define (fn-list-0-n-1 n b)
  (cond
    [(zero? n) '()]
    [else
     (... b
           (fn-list-0-n-1 (... n) (... b)))]))

(define (list-0-n-1 n b)
  (cond
    [(zero? n) '()]
    [else
     (cons b
           (list-0-n-1 (sub1 n) (add1 b)))]))

