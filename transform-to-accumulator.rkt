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


























