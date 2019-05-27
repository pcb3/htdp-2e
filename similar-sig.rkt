;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname similar-sig) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 15.2 Similarities in Signatures

;; Exercise 254. Formulate signatures for the following functions:

; [List-of Number] [Number Number -> Boolean] -> [List-of Number] 

; [List-of String] [String String -> Boolean] -> [List-of String]

; [x Y] [List-of X] [X X -> Y] -> [List-of X]

; [List-of IR] [IR IR -> Boolean] -> [List-of IR]

;; Exercise 254

; [List-of Number] [Number -> Number] -> [List-of Number]

; [List-of String] [String -> String] -> [List-of String]

; [X] [List-of X] [X -> X] -> [List-of X]

; [List-of IR] [String -> String] -> [List-of String]