;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mathematical-exampples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 28 Mathematical Examples

(require htdp/graphing)

;; 28.1 Newton's Method

; [Number -> Number] Number -> Number
; consumes a function f and a Number n and
; produces the slope of f at n

(check-expect (slope (lambda (x) 1) 10) 0)
(check-expect
 (slope
  (lambda (x) (+ x 1)) 1) 1)
;(graph-fun (lambda (x) (+ x 1)) 'blue)

(check-expect
 (slope (lambda (x) (+ (* 2 x) 1)) 1) 2)
;(graph-fun (lambda (x) (+ (* 2 x) 1)) 'green)

(define EPSILON 0.001)

(define (slope f n)
  (* (/ 1 (* 2 EPSILON))
     (- (f (+ n EPSILON)) (f (- n EPSILON)))))


;;====
;; 456

; [Number -> Number] Number -> Number
; consumes a function f and a Number n and produces the root
; of the tangent of f at n

(check-error
 (root-of-tangent (lambda (x) 1) 10) ERROR)
(check-expect (root-of-tangent (lambda (x) (+ x 1)) 1) -1)
(check-expect
 (root-of-tangent (lambda (x) (- (* 2 x) 1)) 1) 1/2)

(define (fn-root-of-tangent f n)
  (cond
    [(zero? (slope f n)) ...]
    [else (- n (/ (f n) (slope f n)))]))

(define (root-of-tangent f n)
  (cond
    [(zero? (slope f n)) (error ERROR)]
    [else (- n (/ (f n) (slope f n)))]))

(define ERROR "undefined")

;;====
;; 457

; Number -> Number
; consumes a Number p and r and produces the
; time in months it takes to be within epsilon of double p at
; r % interest
; generative: each step we get closer to the target amount
; terminate: does not terminate for 0% interest rate

(define epsilon 1)

(define (double-amount p r)
  (local
    ((define doubled (* 2 p)))
    (cond
      [(zero? r) (error "infinite loop")]
      [else (compound p r 0 doubled)])))

(define (compound v r n final)
  (cond
    [(<= final v) n]
    [else
     (compound (+ v (* v (/ r 100))) r (add1 n) final)]))

































