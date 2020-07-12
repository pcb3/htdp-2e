;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname backtracking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 29.2 Project: Backtracking

(require 2htdp/image)

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

;;====
;; 479

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

;;====
;; 480

(define WIDTH 100)
(define HEIGHT 100)

(define MT (empty-scene WIDTH HEIGHT))
(define QUEEN
  (crop/align "center" "center" 50 50
              (scale 0.1 (bitmap/file "chess-queen.png"))))
(define (create-square n)
  (square (/ WIDTH n) "outline" "black"))

; Number Image -> Image
; consumes a Number n and an Image img produces n squares
; in a row
(check-expect
 (create-row 2 2)
 (beside (create-square 2) (create-square 2)))
(check-expect
 (create-row 4 4)
 (beside (create-square 4) (create-square 4)
         (create-square 4) (create-square 4)))

(define (fn-create-row n side)
  (cond
    [(= 1 n) ...]
    [else
     (beside
      (create-square side) (fn-create-row (sub1 n) side))]))

(define (create-row n side)
  (cond
    [(= 1 n) (create-square side)]
    [else
     (beside (create-square side)
             (create-row (sub1 n) side))]))
 

















