;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mathematical-exampples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 28 Mathematical Examples

;(require htdp/graphing)

;; 28.1 Newton's Method

; [Number -> Number] Number -> Number
; consumes a function f and a number n and
; produces the slope of f at n

(check-expect (slope (lambda (x) 1) 10) 0)
(check-expect
 (slope
  (lambda (x) (+ x 1)) 1) 1)
;(graph-fun (lambda (x) (+ x 1)) 'blue)

(check-expect
 (slope (lambda (x) (+ (* 2 x) 1)) 1) 2)
;(graph-fun (lambda (x) (+ (* 2 x) 1)) 'green)

(define EPSILON 0.001)

(define (slope f n)
  (* (/ 1 (* 2 EPSILON))
     (- (f (+ n EPSILON)) (f (- n EPSILON)))))