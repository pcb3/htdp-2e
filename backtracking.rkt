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

; Number Number -> Image
; consumes two Numbers n and side, and produces n squares
; in a row of side length side
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

; N [List-of QP] Image -> Image
; consumes a natural number n, a list of QP's loqp and an
; Image img and produces an Image of a chess board with the
; given image placed according to given QP's

(check-expect (render-queens 2 '() QUEEN)
              (above (create-row 2 1)
                     (create-row 2 1)))

(check-expect
 (render-queens 2 (list (make-posn 1 1)) QUEEN)
 (place-images
  (list QUEEN)
  (list (make-posn (* (/ WIDTH 2)
                      (posn-x (first (list (make-posn 1 1)))))
                   (* (/ HEIGHT 2)
                      (posn-y (first (list (make-posn 1 1)))))))
  (above (create-row 2 1)
         (create-row 2 1))))

(check-expect
 (render-queens 2 (list (make-posn 1 1)
                        (make-posn 2 2)) QUEEN)
 (place-images
  (list QUEEN QUEEN)
  (list (make-posn
         (* (/ WIDTH 2)
            (posn-x (first (list (make-posn 1 1)
                                 (make-posn 2 2)))))
         (* (/ HEIGHT 2)
            (posn-y (first (list (make-posn 1 1)
                                 (make-posn 2 2))))))
        (make-posn
         (+ (/ WIDTH 2)
            (* (/ WIDTH 2)
               (posn-x (second (list (make-posn 1 1)
                                     (make-posn 2 2))))))
         (+ (/ WIDTH 2)
            (* (/ HEIGHT 2)
               (posn-y (second (list (make-posn 1 1)
                                     (make-posn 2 2))))))))
  (above (create-row 2 1)
         (create-row 2 1))))

(define (fn-render-queens n loqp img)
  (cond
    [(empty? loqp) ...]
    [else
     (place-image
      img
      (+ (/ ... n) (* (/ ... n)
                      (posn-x (first loqp))))
      (+ (/ ... n) (* (/ ... n)
                      (posn-y (first loqp))))
      (fn-render-queens n (rest loqp) img))]))
    
(define (render-queens n loqp img)
  (cond
    [(empty? loqp) (create-row n n)]
    [else
     (place-image
      img
      (+ (/ WIDTH n) (* (/ WIDTH n)
                        (posn-x (first loqp))))
      (+ (/ WIDTH n) (* (/ WIDTH n)
                        (posn-y (first loqp))))
      (render-queens n (rest loqp) img))]))
 

















