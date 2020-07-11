;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname backtracking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 29.2 Project: Backtracking

(define QUEENS 8)
; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, QUEENS).
; interpretation (make-posn r c) denotes the square at the
; r-th row and the c-th column

(define QP0 (make-posn 0 0))
(define QP1 (make-posn 1 0))
(define QP2 (make-posn 0 1))
(define QP3 (make-posn 8 8))

; QP QP -> Boolean
; consumes two QP's and produces true if they are threatening
; each other (on the same horizontal, verticle or diagonal)

(check-expect
 (threatening? (make-posn 0 0) (make-posn 1 0)) #true)
(check-expect
 (threatening? (make-posn 0 0) (make-posn 0 1)) #true)
(check-expect
 (threatening? (make-posn 0 0) (make-posn 1 1)) #true)
(check-expect
 (threatening? (make-posn 1 0) (make-posn 0 1)) #true)
(check-expect
 (threatening? (make-posn 0 0) (make-posn 2 1)) #false)
(check-expect
 (threatening? (make-posn 1 2) (make-posn 2 0)) #false)

(define (fn-threatening? qp1 qp2)
  (local
    ((define row-threat? (= (posn-x qp1) (posn-x qp2)))
     (define column-threat? (= (posn-y qp1) (posn-y qp2)))
     (define pos-diagonal-threat?
             (= (+ (posn-x qp1) (posn-y qp1))
                (+ (posn-x qp2) (posn-y qp2))))
     (define neg-diagonal-threat?
             (= (- (posn-x qp1) (posn-y qp1))
                (- (posn-x qp2) (posn-y qp2)))))
    (or row-threat? column-threat? pos-diagonal-threat?
        neg-diagonal-threat?)))

(define (threatening? qp1 qp2)
  (local
    ((define row-threat? (= (posn-x qp1) (posn-x qp2)))
     (define column-threat? (= (posn-y qp1) (posn-y qp2)))
     (define pos-diagonal-threat?
             (= (+ (posn-x qp1) (posn-y qp1))
                (+ (posn-x qp2) (posn-y qp2))))
     (define neg-diagonal-threat?
             (= (- (posn-x qp1) (posn-y qp1))
                (- (posn-x qp2) (posn-y qp2)))))
    (or row-threat? column-threat? pos-diagonal-threat?
        neg-diagonal-threat?)))