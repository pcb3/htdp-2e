;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname append-from-fold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 272

; List List -> List
; consumes two lists and concatenates them

(check-expect (append-from-fold '() '()) '())

(check-expect (append-from-fold '(1) '(2)) '(1 2))

(check-expect (append-from-fold '(1) '(2 3))
              '(1 2 3))

(define (fn-append-from-fold l1 l2)
  (foldr ... l2 l1))

(define (append-from-fold l1 l2)
  (foldr cons l2 l1))
