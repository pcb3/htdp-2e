;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Scope

(require 2htdp/abstraction)

; design enumerate

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index

(check-expect (enumerate '()) '())
(check-expect (enumerate '(a b c))
              (list (list 'a 0) (list 'b 1) (list 'c 2)))

(define (fn-enumerate lx)
  (local ((define index
            (... (length lx) (lambda (...) ...))))
    (... ... lx ...)))

(define (enumerate lx)
  (local ((define index
            (build-list (length lx)
                        (lambda (i) i))))
    (map list lx index)))