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
(define QUEEN
  (crop/align "center" "center" 50 50
              (scale 0.1 (bitmap/file "chess-queen.png"))))
(define (create-square n)
  (square (/ WIDTH n) "outline" "black"))

(define LOQP1 (list (make-posn 0 0)))
(define LOQP2 (list (make-posn 0 0)
                    (make-posn 1 1)))
(define LOQP3 (list (make-posn 0 0)
                    (make-posn 1 1)
                    (make-posn 2 2)))

; N [List-of QP] Image -> Image
; consumes a natural number n, a list of QP's loqp and an
; Image img and produces an Image of a chess board with the
; given image placed according to given QP's

(check-expect
 (render-queens-local 2 '() QUEEN)
 (place-image
  (above (beside (create-square 2)
                 (create-square 2))
         (beside (create-square 2)
                 (create-square 2)))
  (image-width QUEEN)
  (image-width QUEEN)
  (empty-scene (* (image-width QUEEN) 2)
               (* (image-width QUEEN) 2))))

(check-expect
 (render-queens-local 2 LOQP1 QUEEN)
 (place-image
  QUEEN
  (+ (/ (image-width QUEEN) 2)
     (* (image-width QUEEN) (posn-x (first LOQP1))))
  (+ (/ (image-width QUEEN) 2)
     (* (image-width QUEEN) (posn-y (first LOQP1))))
  (place-image
   (above (beside (create-square 2)
                  (create-square 2))
          (beside (create-square 2)
                  (create-square 2)))
   (image-width QUEEN)
   (image-width QUEEN)
   (empty-scene (* (image-width QUEEN) 2)
                (* (image-width QUEEN) 2)))))

(define (render-queens-local n loqp img)
  (local
    ((define SIZE (image-width QUEEN))
     (define create-square
       (square (image-width QUEEN) "outline" "black"))     
     (define MTS (empty-scene (* n SIZE) (* n SIZE)))
     (define (create-row n)
       (cond
         [(= 1 n) create-square]
         [else
          (beside create-square
                  (create-row (sub1 n)))]))
     (define (create-board rows)
       (cond
         [(= 1 rows) (create-row n)]
         [else
          (above (create-row n)
                 (create-board (sub1 rows)))])))
    (cond
      [(empty? loqp)
       (place-image
        (create-board n)
        (/ (image-width MTS) 2)
        (/ (image-width MTS) 2) MTS)]
      [else
       (place-image
        img
        (+ (/ SIZE 2) (* SIZE (posn-x (first loqp))))
        (+ (/ SIZE 2) (* SIZE (posn-y (first loqp))))
        (render-queens-local n (rest loqp) img))])))


;;========================
;; valid 4-queen solutions

(define 4-QUEEN1 (list (make-posn 1 0)
                       (make-posn 3 1)
                       (make-posn 0 2)
                       (make-posn 2 3)))

(define 4-QUEEN2 (list (make-posn 2 0)
                       (make-posn 0 1)
                       (make-posn 3 2)
                       (make-posn 3 1)))

;;====
;; 481

; N -> Boolean
; consumes a Natural number n and produces a predicte to test
; if a queen placement is a solution to the n queens problem

(check-expect (n-queens-solution? n) ...)

(define (fn-n-queens-solution? n)
  (lambda (x)
    (local
      ((define length-of-list n)

       (define valid? (andmap (lambda (y))))))))
       

(define (n-queens-solution? n) ...)












