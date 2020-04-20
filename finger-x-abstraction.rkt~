;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname finger-x-abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 16.7 Finger exercises: Abstraction

; exercise 267

; [List-of Number] Number -> [List-of Number]
; converts a list of US$ amounts into Euro's
; based off exchange rate of US$1.06 per Euro

(check-expect (convert-euro '() 1.06) '())

(check-expect
 (convert-euro (list 1 11.05) 1.06)
 (list 1.06 11.713))

(define (fn-convert-euro lon ex)
  (local (; Number Number -> Number
          ; ...
          (define (x n)
            ...))
    (map x lon)))
          

(define (convert-euro lon ex)
  (local (; Number Number -> Number
          ; finds the product of two numbers
          (define (x n)
            (* n ex)))
    (map x lon)))

; [List-of Number] -> [List-of Number]
; converts Fahrenheit to Celcius
(check-expect (convertFC '()) '())

(check-expect
 (convertFC '(1 2 100))
 (list (* (- 1 32) (/ 5 9))
       (* (- 2 32) (/ 5 9))
       (* (- 100 32) (/ 5 9))))

(define (fn-convert lon)
  (local (; Number -> Number
          ; ...
          (define (f-to-c n)
            ...))
    (map f-to-c lon)))

(define (convertFC lon)
  (local (; Number -> Number
          ; applies F to C conversion
          (define (f-to-c n)
            (* (- n 32) (/ 5 9))))
    (map f-to-c lon)))

; [List-of Posn] -> [List-of LoPN]
; consumes a list of x and y coordinates and outputs
; a list of lists of pair of numbers

(check-expect (translate '()) '())

(check-expect
 (translate (list (make-posn 0 0)
                  (make-posn 10 10)
                  (make-posn 99 999)))
 (list (list 0 0)
       (list 10 10)
       (list 99 999)))

(define (fn-translate lop)
  (local (; Posn -> List-of-Numbers
          ; ...
          (define (extract-pair l)
            ...))
    (map extract-pair lop)))

(define (translate lop)
  (local (; Posn -> List-of-Numbers
          ; consumes a posn, extracts the coords
          ; and creates a new list with x/y pair
          (define (extract-pair l)
            (list (posn-x l) (posn-y l))))
    (map extract-pair lop)))
                
; exercise 268

(define-struct IR [name desc buy sell])
; An IR is a structure
; (make-IR String String Number Number)
; it is a record of an inventory item with a name,
; description, acquisition and sale price in $

(define IR1 (make-IR "giraffe"
                     "herbivore"
                     1000 2500))

(define IR2 (make-IR "rhino"
                     "herbivore"
                     5000 9999))

(define IR3 (make-IR "snake"
                     "carnivore"
                     899 3000))

(define LOR (list IR1 IR3 IR2))

; List-of-IR -> List-of-IR
; consumes a list of iventory records and produces
; a sorted list by the difference in acquisition
; and sale prices

(check-expect (sort-ir '()) '())

(check-expect
 (sort-ir LOR)
 (list
 (make-IR "giraffe" "herbivore" 1000 2500)
 (make-IR "snake" "carnivore" 899 3000)
 (make-IR "rhino" "herbivore" 5000 9999)))

(define (fn-sort-ir lor)
  (local (; IR -> Number
          ;...
          (define (dif ir)
            ...))
    (sort (dif lor) <)))

(define (sort-ir lor)
  (local (; IR -> Boolean
          ; compares the difference of two records
          ; buy and sale price and returns true if
          ; <=
          (define (dif<= a b)
            (<=
             (- (IR-sell a)
                (IR-buy a))
             (- (IR-sell b)
                (IR-buy b)))))
    (sort lor dif<=)))































