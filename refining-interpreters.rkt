;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-interpreters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 21 Refining Interpreters

; 21.1 Interpreting Expressions

(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; exercise 345

; A BSL-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; (+ 10 -10) -> (make-add 10 -10)
; (+ (* 20 3) 33) -> (make-add (make-mul 20 3) 33)
; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))) ->
; (make-add (make-mul 3.14 (make-mul 2 3))
;           (make-mul 3.14 (make-mul -1 -9)))

; (make-add -1 2) -> (+ -1 2)
; (make-add (make-mul -2 -3) 33) -> (+ (* -2 -3) 33)
; (make-mul (make-add 1 (make-mul 2 3)) 3.14) ->
; (* (+ 1 (* 2 3)) 3.14)

; Exercise 346

; an BSL-eval is one of:
; - Number
; - a structure (make-add [BSL-expr BSL-expr])
; - a structure (make-mul [BSL-expr BSL-expr])

(define BSL-EVAL1 (make-add 1 1))
(define BSL-EVAL2 (make-mul 2 3))
(define BSL-EVAL3 (make-add
                   (make-mul BSL-EVAL2 BSL-EVAL1)
                   (make-add BSL-EVAL1 BSL-EVAL1)))

; Exercise 347

; BSL-eval -> Number
; consumes a BSL expression that can evaluate and
; produces that value

(check-expect (eval-expression BSL-EVAL1) (+ 1 1))
(check-expect (eval-expression BSL-EVAL2) (* 2 3))
(check-expect (eval-expression BSL-EVAL3)
              (+ (* (* 2 3) (+ 1 1))
                 (+ (+ 1 1) (+ 1 1))))
(check-expect (eval-expression '$)
              "not a BSL expression")

(define (fn-eval-expression bsl-eval)
  (cond
    [(number? bsl-eval) bsl-eval]
    [(add? bsl-eval)
     (... (fn-eval-expression (add-left bsl-eval))
          (fn-eval-expression (add-right bsl-eval)))]
    [(mul? bsl-eval)
     (... (fn-eval-expression (mul-left bsl-eval))
          (fn-eval-expression (mul-right bsl-eval)))]
    [else ...]))
  
(define (eval-expression bsl-eval)
  (cond
    [(number? bsl-eval) bsl-eval]
    [(add? bsl-eval)
     (+ (eval-expression (add-left bsl-eval))
        (eval-expression (add-right bsl-eval)))]
    [(mul? bsl-eval)
     (* (eval-expression (mul-left bsl-eval))
        (eval-expression (mul-right bsl-eval)))]
    [else "not a BSL expression"]))

; Exercise 348

(define-struct and-expr [left right])
(define-struct or-expr [left right])
(define-struct not-expr [bool])

; A BBSL (short for Boolean BSL) is one of:
; - #true
; - #false
; - and-expr
; - or-expr
; - not-expr

(define BBSL1 #true); -> #true
(define BBSL2 #false); -> #false
(define BBSL3 (make-and-expr #true #false)); -> #false
(define BBSL4 (make-or-expr #true #false)); -> #true
(define BBSL5 (make-not-expr #true)); -> #false

; BBSL -> Boolean
; consumes a representation of BBSL bbsl and
; produces the Boolean value

(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression (make-and-expr #true #true)) (and #true #true))
(check-expect (eval-bool-expression (make-and-expr #true #false)) (and #true #false))
(check-expect (eval-bool-expression (make-or-expr #true #false)) (or #true #false))
(check-expect (eval-bool-expression (make-or-expr #false #false)) (or #false #false))
(check-expect (eval-bool-expression (make-not-expr #true)) (not #true))
(check-expect (eval-bool-expression (make-not-expr #false)) (not #false))
;(check-expect (eval-bool-expression (make-and-expr #true (make-not-expr #true))) #false)

(define (eval-bool-expression bbsl)
  (cond
    [(boolean? bbsl) bbsl]
    [(and-expr? bbsl) (and (eval-bool-expression (and-expr-left bbsl))
                           (eval-bool-expression (and-expr-right bbsl)))]
    [(or-expr? bbsl) (or (eval-bool-expression (or-expr-left bbsl))
                         (eval-bool-expression (or-expr-right bbsl)))]
    [(not-expr? bbsl) (not (eval-bool-expression (not-expr-bool bbsl)))]
    [else "not a BBSL expression"]))






























