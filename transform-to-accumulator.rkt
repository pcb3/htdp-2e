;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname transform-to-accumulator) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 32.3 Transforming Functions into Accumulator Style

(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))


; Tree -> Number
; consumes a binary Tree abt and produces the height

(check-expect (height '()) 0)
(check-expect (height (make-node
                       (make-node '() '()) '())) 2)

(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

;;====
;; 498

(define ABT0 '())
(define ABT1 (make-node '() '()))
(define ABT2 (make-node
              (make-node '() '())
              '()))
(define ABT3 (make-node
              (make-node
               (make-node '() '()) '())
              '()))
(define ABT4 (make-node
              (make-node
               (make-node '() (make-node '() '())) '())
              '()))

; Tree -> N
; measures the height of abt0

(check-expect (height.v2 ABT0) 0)
(check-expect (height.v2 ABT1) 1)
(check-expect (height.v2 ABT2) 2)
(check-expect (height.v2 ABT3) 3)
(check-expect (height.v2 ABT4) 4)

(define (height.v2 abt0)
  (local (; Tree N -> N
          ; measures the height of abt
          ; accumulator a is the number of steps 
          ; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
               (max
                (height/a (node-left abt)  (+ a 1))
                (height/a (node-right abt) (+ a 1)))])))
    (height/a abt0 0)))

; Tree -> N
; measures height of abt0

(check-expect (height.v3 ABT0) 0)
(check-expect (height.v3 ABT1) 1)
(check-expect (height.v3 ABT2) 2)
(check-expect (height.v3 ABT3) 3)
(check-expect (height.v3 ABT4) 4)

(define (fn-height.v3 abt0)
  (local
    (; Tree N N -> N
     ; measures the height of abt
     ; accumulator s is the number of steps 
     ; it takes to reach abt from abt0
     ; accumulator m is the maximal height of
     ; the part of abt0 that is to the left of abt
     (define (h/a abt s m)
       (cond
         [(empty? abt) ...]
         [else
          (...
           (h/a (node-left abt)
                s ... ... m ...) ...
                                 ... (h/a (node-right abt)
                                          ... s ... ... m ...) ...)])))
    (h/a abt0 0 0)))

(define (height.v3 abt0)
  (local
    (; Tree N N -> N
     ; measures the height of abt
     ; accumulator s is the number of steps 
     ; it takes to reach abt from abt0
     ; accumulator m is the maximal height of
     ; the part of abt0 that is to the left of abt
     (define (h/a abt s m)
       (cond
         [(empty? abt) (+ s m)]
         [else
          (max 
           (h/a (node-left abt) s (add1 m))
           (h/a (node-right abt) (add1 s) m))])))
    (h/a abt0 0 0)))

;;====
;; 499

; List-of Number -> Number
; consumes a list of Number lon0 and produces the product

(check-expect (product/a '(1 2)) 2)
(check-expect (product/a '(1 2 3)) 6)
(check-expect (product/a '(0 1 2)) 0)

(define (fn-product/a lon0)
  (local
    (; lon0 lon -> Number
     ; computes the product
     ; lon is the list of numbers left to be multiplied
     ; accumulator a is the product of numbers computed so far
     (define (p/a lon a)
       (cond
         [(empty? lon) ...]
         [else
          (p/a (rest lon) (* (first lon) a))])))
    (p/a lon0 1)))

(define (product/a lon0)
  (local
    (; lon0 lon -> Number
     ; computes the product
     ; lon is the list of numbers left to be multiplied
     ; accumulator a is the product of numbers computed so far
     (define (p/a lon a)
       (cond
         [(empty? lon) a]
         [else
          (p/a (rest lon) (* (first lon) a))])))
    (p/a lon0 1)))


;; non-accumulator product

(define (product lon)
  (cond
    [(empty? lon) 1]
    [else
     (* (first lon) (product (rest lon)))]))

;;====
;; 500

; List -> Number
; consumes a List an produces the number of items in that list

(check-expect (how-many '()) 0)
(check-expect (how-many '(1 2 3)) 3)

(define (fn-how-many l0)
  (local
    (; List Number -> Number
     ; produces the length of l
     ; accumulator a is the number of items observed so far
     ; l is the current list
     (define (how-many/a l a)
       (cond
         [(empty? l) ...]
         [else
          (how-many/a (rest l) (add1 a))])))

    (how-many/a l0 0)))

(define (how-many l0)
  (local
    (; List Number -> Number
     ; produces the length of l
     ; accumulator a is the number of items observed so far
     ; l is the current list
     (define (how-many/a l a)
       (cond
         [(empty? l) a]
         [else
          (how-many/a (rest l) (add1 a))])))

    (how-many/a l0 0)))

;;====
;; 501

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N -> Number
; adds n to pi without using +

(check-within (add-to-pi.v2 0) pi 0.1)
(check-within (add-to-pi.v2 1) (+ 1 pi) 0.1)

(define (add-to-pi.v2 n)
  (local
    (; N Number -> Number
     ; adds n to pi
     ; accumulator a adds 1 to the current subtotal
     (define (add-to-pi/a n a)
       (cond
         [(zero? n) (+ pi a)]
         [else
          (add-to-pi/a (sub1 n) (add1 a))])))

    (add-to-pi/a n 0)))

;;====
;; 502

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror.v2 (explode "abc")) (explode "abcba"))

(define (mirror.v2 s0)
  (local
    (; [NElist-of 1String] [NEList-of 1String] -> [NEList-of 1String]
     ; creates a palindrome from s0
     ; accumulator a is the list of 1Strings compiled so far
     (define (mirror/a s a)
       (cond
         [(empty? s) (append (reverse a) (reverse (all-but-last s0)))]
         [else
          (mirror/a (rest s) (cons (first s) a))])))

    (mirror/a s0 '())))

;;===
;; v3

(check-expect
 (mirror.v3 (explode "abc")) (explode "abcba"))

(define (mirror.v3 s0)
  (local
    ((define (mirror/a s a)
       (cond
         [(empty? (rest s)) (append (reverse a) s a)]
         [else
          (mirror/a (rest s) (cons (first s) a))])))

    (mirror/a s0 '())))

; Polygon -> Posn
; extracts the last item from p
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; [NEList-of 1String] -> [NEList-of 1String]
; consumes a List of 1String's and produces all but the last item

(check-expect (all-but-last (explode "abc")) (explode "ab"))

(define (fn-all-but-last nlon)
  (cond
    [(empty? (rest nlon)) ...]
    [else
     (cons (first nlon) (fn-all-but-last (rest nlon)))]))

(define (all-but-last nlon)
  (cond
    [(empty? (rest nlon)) '()]
    [else
     (cons (first nlon) (all-but-last (rest nlon)))]))

;;====
;; 503

; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))

(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 1 2) (0 4 5) (0 8 9)))
             "error: all first coefficients are zero")

(define (rotate M)
  (local
    ((define get-firsts (map (lambda (x) (first x)) M)))
    (cond
      [(andmap (lambda (y) (zero? y)) get-firsts)
       (error "error: all first coefficients are zero")]
      [(not (zero? (first (first M))))
       (cons (first M) (rest M))]
      [else
       (rotate (append (rest M) (list (first M))))])))

(define (rotate.v2 M0)
  (local (; Matrix ... -> Matrix 
          ; accumulator seen is the rows seen so far
          (define (rotate/a M seen)
            (cond
              [(empty? M) '()]
              [(not (zero? (first (first M))))
               (cons (first M) (append seen (rest M)))]
              [else (rotate/a (rest M)
                              (cons (first M) seen))])))
    (rotate/a M0 '())))

;;====
;; 504

; List-of N -> Number
; consumes a list of natural Numbers lon and produces the
; corresponding number

(check-expect (to10 '(0)) 0)
(check-expect (to10 '(1 2 0)) 120)

(define (to10 lon0)
  (local
    ((define (to10/a a lon)
       (cond
         [(empty? lon) a]
         [else
          (to10/a (+ (* (first lon) (expt 10 (sub1 (length lon)))) a)
                  (rest lon))])))

    (to10/a 0 lon0)))

;;====
;; 505

; N [>=1] -> Boolean
; determines whether n is a prime number
(check-expect (is-prime? 5) #true)
(check-expect (is-prime? 42) #false)
(check-expect (is-prime? 1) #false)

(define (is-prime? n0)
  (local
    (; accumulator a is the N to be tested for primality
     (define (is-prime?/a a n)
       (cond
         [(= n 1) #true]     
         [(integer? (/ a n)) #false]             
         [else (is-prime?/a a (sub1 n))])))
    (cond
      [(= n0 1) #false]
      [else
       (is-prime?/a n0 (sub1 n0))])))

;;====
;; 506

; List-of X -> List of Y
; consumes a list l and applies a function f to each item on the list

(check-expect (my-map (lambda (x) x) '()) '())
(check-expect (my-map add1 '(1 2 3)) '(2 3 4))
(check-expect (my-map even? '(0 1 2)) '(#true #false #true))

(define (fn-my-map f l)
  (cond
    [(empty? l) ...]
    [else (... (f (first l)) (fn-my-map f (rest l)))]))

(define (my-map f l)
  (cond
    [(empty? l) '()]
    [else (cons (f (first l)) (my-map f (rest l)))]))

;;====
;; accumulator version of map


(check-expect (map-accumulator (lambda (x) x) '()) '())
(check-expect (map-accumulator add1 '(1 2 3)) '(2 3 4))
(check-expect (map-accumulator even? '(0 1 2)) '(#true #false #true))

(define (fn-map-accumulator f l0)
  (local
    (; accumulator a is the application of f to l so far
     (define (map-accumulator/a f l a)
       (cond
         [(empty? l) ...]
         [else
          (map-accumulator/a f (rest l) (... (f (first l)) a))])))
    (map-accumulator/a f l0 '())))

(define (map-accumulator f l0)
  (local
    (; accumulator a is the application of f to l so far
     (define (map-accumulator/a f l a)
       (cond
         [(empty? l) (reverse a)]
         [else
          (map-accumulator/a f (rest l) (cons (f (first l)) a))])))
    (map-accumulator/a f l0 '())))
     


























