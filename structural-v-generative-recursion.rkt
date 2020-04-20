;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname structural-v-generative-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 26.3 Structural versus Generative Recursion

;;====
;; 437

; [List-of X] -> Number
; consumes a list P and returns its length
(check-expect (special-length '()) 0)
(check-expect (special-length (explode "abcdefg")) 7)

(define (special-length P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special-length (rest P)))]))

; [List-of X] -> Number
; consumes a List and produces its length
(define (solve p) 0)

; [List-of X] Number -> Number
; consumes a list l and a number n and adds 1 to the
; result of n
(check-expect (combine-solutions '("a" "b" "c") 0) 1)

(define (combine-solutions l n) (add1 n))

;;======

; [List-of X] -> Number
; consumes a list P and returns its length
(check-expect (special-negate '()) '())
(check-expect (special-negate '(0 1 2 -3))
              '(0 -1 -2 3))

(define (special-negate P)
  (cond
    [(empty? P) (solve-negate P)]
    [else
     (combine-solutions-negate
       P
       (special-negate (rest P)))]))

(define (solve-negate p) '())

(define (combine-solutions-negate l lon)
  (cons (* (first l) -1) lon))

;;======

; [List-of X] -> Number
; consumes a list P and returns its length
(check-expect (special-upper '()) '())
(check-expect (special-upper '("oh" "hai" "ther"))
              '("OH" "HAI" "THER"))

(define (special-upper P)
  (cond
    [(empty? P) (solve-upper P)]
    [else
     (combine-solutions-upper
       P
       (special-upper (rest P)))]))

(define (solve-upper p) '())

(define (combine-solutions-upper l los)
  (cons (string-upcase (first l)) los))



















