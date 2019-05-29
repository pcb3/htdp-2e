;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname using-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 16 Using Abstractions

;; exercise 256: Explain the follwing abstract function

; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(define (argmax f lx) ...)

; takes a function f and a non-empty list, applies f to the first item,
; and the second item in the list, takes the maximum of the two
; items when f is applied, and tests the next item.
; returns the item who has the maximum value when f is applied
; in the list

; [Number -> Number] [NEList-of Number] -> Number

; argmin:
; takes a function f and a non-empty list and applies f to the first
; and second item, takes the minimum. takes the item that had the
; minimum value and tests it against the next item in the list.
; returns the minimum item
