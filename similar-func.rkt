;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname similar-func) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 14.1 similarities in funcitons

; String LoS -> Boolean
; determines wether l contains s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else
     (or (string=? (first l) s)
         (contains? s (rest l)))]))

;; ex 235

; LoS -> Boolean
; determines wether l contains "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; LoS -> Boolean
; determines wether l contains "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; LoS -> Boolean
; determines wether l contains "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))

;; ex 236

; Lon -> Lon
; adds 1 to each item on l

(check-expect (add1* '()) '())

(check-expect (add1* '(1 2 3)) '(2 3 4))

(check-expect (add1* '(0)) '(1))

(define (add1* l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (add1 (first l))
      (add1* (rest l)))]))

; Lon -> Lon
; adds 5 to each item on l

(check-expect (plus5 '()) (add-5 '()))

(check-expect (plus5 '(0)) (add-5 '(0)))

(check-expect (plus5 '(1 2)) (add-5 '(1 2)))

(define (plus5 l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l) 5)
      (plus5 (rest l)))]))

; Number LoN -> LoN
; adds n to each item in l

(check-expect (addn 0 '()) '())

(check-expect (addn 0 '(1 2 3)) '(1 2 3))

(check-expect (addn 10 '(1 2)) '(11 12))

(define (fn-addn n l)
  (cond
    [(empty? l) ...]
    (... (... (... n (first l))
         (fn-addn n (rest l))))))

(define (addn n l)
  (cond
    [(empty? l) '()]
    [else
     (cons (+ n (first l))
              (addn n (rest l)))]))

(define (add-1 l)
  (addn 1 l))

(define (add-5 l)
  (addn 5 l))

(define (sub-2 l)
  (addn -2 l))

;; ex 237

; Number Number -> Boolean
; is the area of a square with side x larger than c

(define (squared>? x c)
  (> (* x x) c))

;; ex 238

; R Nelon -> Number
; consumes a non-empty list of numbers l, and an operator R, and outputs
; a number that fits the criteria of R

(define (fn-extracted R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [...
     (... (R (first l)
            (fn-extracted R (rest l)))
         (first l)
         (fn-extracted R (rest l)))]))
             
(define (extracted R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (extracted R (rest l)))
         (first l)
         (extracted R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
	

(define (inf-1 l)
  (extracted < l))

(define (sup-1 l)
  (extracted > l))

;(sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;      12 11 10 9 8 7 6 5 4 3 2 1))
 
;(sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;      17 18 19 20 21 22 23 24 25))



















