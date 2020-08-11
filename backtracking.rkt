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

; valid 4-queen solutions

(define 4-QUEEN1 (list (make-posn 1 0)
                       (make-posn 3 1)
                       (make-posn 0 2)
                       (make-posn 2 3)))

(define 4-QUEEN2 (list (make-posn 2 0)
                       (make-posn 0 1)
                       (make-posn 3 2)
                       (make-posn 1 3)))

; invalid 4-queen solution

(define 4-QUEEN3 (list (make-posn 2 0)
                       (make-posn 0 1)
                       (make-posn 3 2)
                       (make-posn 1 0)))

;;====
;; 481

; N -> Boolean
; consumes a Natural number n and produces a predicte to test
; if a queen placement is a solution to the n queens problem

(check-expect ((n-queens-solution? 4) 4-QUEEN1) #true)
(check-expect ((n-queens-solution? 4) 4-QUEEN2) #true)
(check-expect ((n-queens-solution? 4) 4-QUEEN3) #false)

(check-satisfied 4-QUEEN1 (n-queens-solution? 4))
(check-satisfied 4-QUEEN2 (n-queens-solution? 4))

(define (fn-n-queens-solution? n)
  (lambda (m)
    (local
      ((define (ormap-threatening? qp1 qp2)
         (andmap (lambda (p) (threatening? qp1 p)) qp2))

       (define (check-threatening loqp)
         (cond
           [(empty? (rest loqp)) #true]
           [(boolean=? #true
                       (ormap-threatening? (first loqp) (rest loqp)))
            #false]
           [else
            (check-threatening (rest loqp))])))
      (cond
        [else (if (check-threatening m)
                  #true
                  #false)]))))

(define (n-queens-solution? n)
  (lambda (m)
    (local
      ((define (ormap-threatening? qp1 qp2)
         (andmap (lambda (p) (threatening? qp1 p)) qp2))

       (define (check-threatening loqp)
         (cond
           [(empty? (rest loqp)) #true]
           [(boolean=? #true
                       (ormap-threatening? (first loqp) (rest loqp)))
            #false]
           [else
            (check-threatening (rest loqp))])))
      (cond
        [else (if (check-threatening m)
                  #true
                  #false)]))))

;;======
;; set=?

(define 4-QUEEN4 (list (make-posn 2 3)
                       (make-posn 0 2)
                       (make-posn 3 1)
                       (make-posn 1 0)))

; List List -> Boolean
; consumes two LOQP's and determines if they are a set

(check-expect (set=? 4-QUEEN1 4-QUEEN4) #true)
(check-expect (set=? 4-QUEEN1 4-QUEEN2) #false)

(define (fn-set=? loqp1 loqp2)
  (andmap (lambda (x) (member? x loqp2)) loqp1))

(define (set=? loqp1 loqp2)
  (andmap (lambda (x) (member? x loqp2)) loqp1))

;;====
;; 482

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false

(check-expect (place-queens (list '()) 3) #false)
(check-expect (place-queens (list '()) 4) 4-QUEEN1)
(check-expect (place-queens (list '()) 4) 4-QUEEN2)

(define (fn-place-queens a-board n)
  (local
    (; setup initial board
     (define setup (board0 n))

     ; places queen if spot avialable
     (define (place-queen-on-open current-board m)
       (local
         ((define list-of-open (find-open-spots current-board)))
         
         (cond
           [(zero? m) current-board]
           [(empty? list-of-open)
            (place-queen-on-open current-board (sub1 m))]
           [else
            (place-queen-on-open
             (add-queen current-board (first list-of-open))
             (sub1 m))])))

     ; compute potential solution 
     (define possible-solution (place-queen-on-open setup n)))

    (cond
      [(not (= (length possible-solution) n))
       #false]
      [else possible-solution])))
       
(define (place-queens a-board n)
  (local
    (; setup initial board
     (define setup (board0 n))

     ; places queen if spot avialable
     (define (place-queen-on-open current-board m)
       (local
         ((define list-of-open (find-open-spots current-board)))
         
         (cond
           [(zero? m) current-board]
           [(empty? list-of-open)
            (place-queen-on-open current-board (sub1 m))]
           [else
            (place-queen-on-open
             (add-queen current-board (first list-of-open))
             (sub1 m))])))

     ; compute potential solution 
     (define possible-solution (place-queen-on-open setup n)))

    (cond
      [(not (= (length possible-solution) n))
       #false]
      [else possible-solution])))

; N -> Board 
; creates the initial n by n board

(check-expect (board0 3) '())
(check-expect (board0 4) '())

(define (board0 n) '())
 
; Board QP -> Board 
; places a queen at qp on a-board

(check-expect (add-queen '() (make-posn 0 0)) (list (make-posn 0 0)))
(check-expect (add-queen (list (make-posn 0 0)) (make-posn 1 1))
              (list (make-posn 1 1) (make-posn 0 0)))

(define (add-queen a-board qp)
  (cons qp a-board))
 
; Board -> [List-of QP]
; finds spots where it is still safe to place a queen

;(check-expect (find-open-spots '()) )

(define (find-open-spots a-board)
  '())

;;====
;; 483

; a Board is a List:
; - '()
; - (cons QP Board)
; A Board is a list of all QP queen placements

(define BOARD0 '())
(define BOARD1 (cons (make-posn 0 0) BOARD0))
(define BOARD2 (cons (make-posn 0 0) (cons (make-posn 1 0) BOARD0)))















