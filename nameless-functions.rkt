;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nameless-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 17 Nameless functions

; exercise 281

(lambda (x) (< x 10))

(lambda (x y) (number->string (* y x)))

(lambda (nat) (cond [(even? nat) 0] [else 1]))

(define-struct ir [name price])
(define IR1 (make-ir "koala" 11))
(define IR2 (make-ir "wallaby" 99))
(define IR3 (make-ir "platypus" 20))

((lambda (cmp inv-r1 inv-r2)
   (cmp (ir-price inv-r1) (ir-price inv-r2)))
 equal? IR2 IR1)

(define DOT (circle 10 "solid" "red"))
(define MT (empty-scene 200 200))

((lambda (pos img)
  (place-image DOT (posn-x pos) (posn-y pos) img))
 (make-posn 100 100) MT)

 
