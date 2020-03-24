;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sl-numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *SL Numbers

;;====
;; 417

; the racket verison is exact and the ISL+ version is inexact

;;====
;; 418

; Number N -> Number
; raises the first number base to the power of the a natural expt
(check-expect (my-expt 0 0) 1)
(check-expect (my-expt 1 0) 1)
(check-expect (my-expt 2 3) 8)
(check-expect (my-expt -2 2) 4)
(check-expect (my-expt -2 3) -8)
(check-expect (my-expt 2 -2) 1/4)

(define (fn-my-expt base expt)
  (cond
    [(zero? ...) ...]
    [(negative? ...)
     (/ (fn-my-expt ... (add1 ...)) ...)]
    [else
     (* (fn-my-expt ... (sub1 ...)) ...)]))

(define (my-expt base expt)
  (cond
    [(zero? expt) 1]
    [(negative? expt)
     (/ (my-expt base (add1 expt)) base)]
    [else
     (* (my-expt base (sub1 expt)) base)]))