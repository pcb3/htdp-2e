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

(define (append-from-foldl l1 l2)
  (foldl cons l2 l1))

; List-of-Numbers -> Number
; consumes a list of numbers and produces the sum

(check-expect (sum-from-fold '(0)) 0)

(check-expect (sum-from-fold '(1)) 1)

(check-expect (sum-from-fold '(1 2 3)) 6)

(define (fn-sum-from-fold l)
  (foldr + (... l) (... l)))

(define (sum-from-fold l)
  (foldr + (first l) (rest l)))

; List-of-Numbers
; consumes a list of numbers and produces the product
(check-expect (product-from-fold '(0)) 0)
(check-expect (product-from-fold '(1)) 1)
(check-expect (product-from-fold '(1 2 2)) 4)

(define (product-from-fold l)
  (foldr * (first l) (rest l)))