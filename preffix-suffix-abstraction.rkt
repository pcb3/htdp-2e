;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname preffix-suffix-abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 274
; use existing abstractions to define prefixes and
; suffixes functions

; List-of-1Strings -> List-of-List-of-1Strings
; consumes a list of 1Strings and produces all
; prefixes of that list using an abstraction

(define (prefix lo1) '())