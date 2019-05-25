;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract-example) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 15 Designing Abstraction

;; 15.1 Abstractions from example

;; exercise 250

; Design tabulate, which is the abstraction of the two functionsin figure 92.
; When tabulate is properly designed, use it to define a tabulation
; function for sqr and tan.

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
	
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number -> [List-of-Number]
; tabulates a mathematical operation between
; n and 0 inclusive in a list

;(check-expect (tab-math 0 sqrt) '(0))
;
;(check-expect (tab-math 4 sqrt)
;              (list 2 #i1.7320508075688772 #i1.4142135623730951 1 0))
;
;(check-expect (tab-math 2 tan)
;              (list #i0.9092974268256817 #i0.8414709848078965 0))

(define (fn-tab-math n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (fn-tab-math (... n) f))]))

(define (tab-math n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tab-math (sub1 n) f))]))

(define (sqrt-from-tab-math n )
  (tab-math n sqrt))

(define (tan-from-tab-math n )
  (tab-math n tan))








