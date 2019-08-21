;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname finger-x-abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; exercise 269

; Number List-of-IR -> List-of-IR
; consumes a threshold, ua, and a list of IR,
; produces a list of IR whose sales price is below
; ua

(check-expect (eliminate-expensive 0 '()) '())

(check-expect
 (eliminate-expensive 3999 LOR)
 (list
  (make-IR "giraffe" "herbivore" 1000 2500)
  (make-IR "snake" "carnivore" 899 3000)))

(define (fn-eliminate-expensive ua lor)
  (local (; Number lor -> Boolean
          ;...
          (define (threshold<? records)
            ...))
    (filter threshold<? lor)))

(define (eliminate-expensive ua lor)
  (local (; Number lor -> Boolean
          ; consumes a threshold and a list of
          ; IR and returns true if the record sell
          ; price is below the threshold t
          (define (threshold<? records)
            (< (IR-sell records) ua)))
    (filter threshold<? lor)))

; String List-of-IR -> List-of-IR
; consumes the name of an inventory item, ty, and
; a list of inventory records and produces a list
; of inventory records that do not use the name ty

(check-expect (recall "" '()) '())

(check-expect
 (recall "snake" LOR)
 (list
  (make-IR "giraffe" "herbivore" 1000 2500)
  (make-IR "rhino" "herbivore" 5000 9999)))
 
(define (fn-recall ty lor)
  (local (; String lor -> Boolean
          ;...
          (define (name-equal? records)
            ...))
    (filter name-equal? lor)))

(define (recall ty lor)
  (local (; String lor -> Boolean
          ; consumes a name and an IR and returns
          ; true if name has not been used 
          (define (name-equal? records)
            (not (equal? (IR-name records) ty))))
    (filter name-equal? lor)))

; List-of-Strings List-of-Strings -> List-of-Strings
; consumes two lists of names and selects all those
; from the second that are also in the first

(check-expect (selection '() '()) '())

(check-expect (selection '(1 2 3) '(3 4 5)) '(3))

(check-expect (selection '(1 2 3) '(4 5 6)) '())

(define (fn-selection l1 l2)
  (local (; List-of-Strings String -> Boolean
          ; ...
          (define (in-both? l s)
            ...))
    (filter in-both? l1)))

(define (selection l1 l2)
  (local (; List-of-Strings -> Boolean
          ; checks s is in l
          (define (in-both? s)
            (member? s l2)))
    (filter in-both? l1)))

; exercise 270

; Number -> List-of-Numbers
; builds a list from 0 to n - 1

(check-expect (build0 0) '())

(check-expect (build0 3) '(0 1 2))

(define (fn-build0 n)
  (local (; Number -> Number
          ;...
          (define (add0 x)
            ...))
    (build-list n add0)))

(define (build0 n)
  (local (; Number -> Number
          ; adds zero to x
          (define (add0 x)
            (+ x 0)))
    (build-list n add0)))

; Number -> List-of-Numbers
; builds a list from 1 to n

(check-expect (buildn 0) '())

(check-expect (buildn 3) '(1 2 3))

(define (buildn n)
  (build-list n add1))

; Number -> List-of-Numbers
; builds a list of 1 / n items

(check-expect (buildQ 0) '())

(check-expect (buildQ 3) '(1 1/2 1/3))

(define (fn-buildQ n)
  (local (; Number -> Number
          ;...
          (define (rationalise q)
            ...))
    (build-list n rationalise)))

(define (buildQ n)
  (local (; Number -> Number
          ; divides 1 by n
          (define (rationalise q)
            (/ 1 (add1 q))))
    (build-list n rationalise)))

; Number -> List-of-Numbers
; builds a list of the first n even items

(check-expect (build-even 0) '())

(check-expect (build-even 3) '(0 2 4))

(define (fn-build-even n)
  (local (; Number -> Number
          ;...
          (define (*2 q)
            ...))
    (build-list n *2)))

(define (build-even n)
  (local (; Number -> Number
          ; consumes a number and returns and even
          (define (*2 g)
            (* 2 g)))
    (build-list n *2)))
            
; Number -> List-of-List-LoN
; creates the identity matrix for n rows

;(check-expect (build-identity 0) '())
;
;(check-expect (build-identity 1) (list '(1)))
;
;(check-expect (build-identity 2) (list
;                                  (list 1 0)
;                                  (list 0 1)))

(define (fn-build-identity n)
  (local (; Number -> LoN
          ;...
          (define (build-row r)
            ...)

          ; Number -> List-LoN
          ;...
          (define (build-m s)
            ...)

          ; Number LoN -> LoN
          ;...
          (define (add-1s t)
            ...)

          ; Number -> Number
          ;...
          (define (zeroed p)
            ...)

          ; List-of-LoN -> List-of-LoN
          ;...
          (define (extract-row lolon)
            (cond
              [(empty? lolon) ...]
              [else
               (cons (... (first lolon))
                     (extract-row (rest lolon)))])))
    
    (build-list n ...)))

(define (build-identity n)
  (local (; Number -> LoN
          ; consumes a number and creates a row of
          ;zero's
          (define (build-row r)
            (build-list n zeroed))

          ; Number -> List-LoN
          ; consumes a number s and builds the zero
          ; matrix for n
          (define (build-m s)
            (build-list n build-row))

          ; Number -> Number
          ; returns 0
          (define (zeroed p)
            (* 0 p)))
                     
    (build-m n)))

; Number -> List-of-LoN
; consumes a number n and returns the identity
; matrix

(define (buidl n)
  (build-list n
              (lambda (i)
                (build-list n
                            (lambda (j)
                              (if (= i j) 1 0))))))





















  