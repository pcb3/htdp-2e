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
(define ABT2 (make-node (make-node '() (make-node '() '()))
                        '()))
(define ABT3 (make-node (make-node (make-node '() '()) '()) '()))

; Tree -> N
; measures the height of abt0
(check-expect (height.v2 example) 3)

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
          (max
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
         [(empty? abt) (max s m)]
         [else
          (max
           (h/a (node-left abt) s (add1 m))
           (h/a (node-right abt) (add1 s) m))])))
    (h/a abt0 0 0)))