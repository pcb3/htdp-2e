;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname representing-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 17.5 Representing with Lambda

(require 2htdp/image)

; A Shape is a function: 
;   [Posn -> Boolean]
; interpretation if s is a shape and p a Posn, (s p) 
; produces #true if p is in s, #false otherwise

; Posn -> Boolean
(lambda (p) (and (= (posn-x p) 3) (= (posn-y p) 4)))

; check that the posn is within half of width and
; height of the center of the shape

; Number Number -> Shape
; represents a point at (x,y)
(define (mk-point x y)
  (lambda (p)
    (and (= (posn-x p) x) (= (posn-y p) y))))
 
(define a-sample-shape (mk-point 3 4))

; an example of applying a posn to mk-point given
; an x and y
(check-expect
 ((mk-point 1 1) (make-posn 1 1)) #true)
(check-expect
 ((mk-point 1 1) (make-posn 3 3)) #false)


; Shape Posn -> Boolean
(define (inside? s p)
  (s p))

; here we a re using inside? to apply the posn p
; to s. s is a function mk-point that has p applied
; as a lambda expression
(check-expect
 (inside? (mk-point 3 4) (make-posn 3 4)) #true)
(check-expect
 (inside? (mk-point 3 4) (make-posn 1 1)) #false)

; Number Number Number -> Shape 
; creates a representation for a circle of radius r
;   located at (center-x, center-y) 
(define (mk-circle center-x center-y r)
  ; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 0)) #true)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn 0 9)) #false)
(check-expect
  (inside? (mk-circle 3 4 5) (make-posn -1 3)) #true)

; exercise 297

; Number Number Posn -> Number
; consumes an x and y coordinate and a position
; and produces the distance between the two points

(check-expect (distance-between 0 0 (make-posn 3 4))
              5)
(check-expect (distance-between 0 0 (make-posn 0 0))
              0)

(define (fn-distance-between x y p)
  (... (... (... (... x (posn-x p)))
           (... (... y (posn-y p))))))

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))












