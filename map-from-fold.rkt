;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname map-from-fold) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 273
; fold from map

; [ X Y ] [ X -> Y] [ List-of X] -> [ List-of Y ]
; consumes a function and a list and applies f to
; each item in the list

(check-expect (map-from-fold add1 '(1 2 3))
              '(2 3 4))

(define (map-from-fold f lx)
  (local (; [X Y] [Y -> Y] -> [List-of X Y] 
          ; consumes a base and an item from a list,
          ; applies f to that item and conses it to
          ; the base
          (define (cons-and-f item base)
            (cons (f item) base)))
    (foldr cons-and-f '() lx)))