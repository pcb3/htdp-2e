;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cost-of-computation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 5: The cost of computation

;; Concrete time, abstract time

;;====
;; 484

; infL requires one recursive call at each step. This is because the
; local expression s stores the value of each call in s that can be
; in the next iteration. So infL uses on the order of n steps.

;;====
;; 485

; a number tree (NT) is one of:
; - Number
; - (cons NT (list NT))

(define NT0 0)
(define NT1 (list 1 2))
(define NT2 (list 4 NT0))
(define NT3 (list NT1 NT2))

; NT -> Number
; consumes a number tree and produces the sum of its numbers

(check-expect (sum-tree NT0) 0)
(check-expect (sum-tree NT1) 3)
(check-expect (sum-tree NT2) 4)
(check-expect (sum-tree NT3) 7)

(define (fn-sum-tree nt)
  (cond
    [(empty? nt) 0]
    [(number? nt) nt]
    [else
     (+ (fn-sum-tree (first nt)) (fn-sum-tree (rest nt)))]))

(define (sum-tree nt)
  (cond
    [(empty? nt) 0]
    [(number? nt) nt]
    [else
     (+ (sum-tree (first nt)) (sum-tree (rest nt)))]))
  








