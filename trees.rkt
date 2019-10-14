;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; IV Intertwined Data

; 19 The Poetry of S-expressions

; 19.1 Trees

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; Exercise 310

; FT -> Number
; consumes an-ftree and counts the child structures
; in the tree

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(check-expect (count-persons Eva) 3)

(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else
     (+ 1
        (count-persons (child-father an-ftree))
        (count-persons (child-mother an-ftree)))]))


        
          












