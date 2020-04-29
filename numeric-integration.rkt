;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname numeric-integration) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 28.2 Numeric Integration

;;====
;; 458

(define ε 0.01)
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 
 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
;(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              ε)
; the ablove test fails becasue it is an polynomial of degree
; >= 2

(define (integrate-kepler f a b)
  (* (/ 1 2) (- b a) (+ (f a) (f b))))

;;====
;; 459

(define R 160)

; [Number -> Number] Number Number -> Number
; consumes a function f and an interval a and b and produces
; the area under the curve between the interval

(check-within (integrate-rectangle
               (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangle
               (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangle
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)

(define (fn-integrate-ractangle f a b)
  (cond
    [(zero? (- b a)) ...]
    [else
     (foldl + ... (map (... f a b)
                       (build-list R ...)))]))    

(define (integrate-rectangle f a b)
  (cond
    [(zero? (- b a)) 0]
    [else
     (foldl + 0
            (map
             (lambda (i)
               (local
                 ((define w (/ (- b a) R)))
                 (* w (f (+ a (* i w) (/ w 2))))))
             (build-list R (lambda (x) x))))]))
            
;;====
;; 460

; [Number -> Number] Number Number -> Number
; consumes a function f and an interval a b and produces
; the area under the interval
; generative: recursively dividing the area up into succesively
; smaller pieces at each step and then applying Kepler's method

(check-within (integrate-dc
               (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc
               (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc
               (lambda (x) (* 3 (sqr x))) 0 10) 1000 ε)

(define threshold 0.01)

(define (fn-integrate-dc f a b)
  (cond
    [(<= (- b a) threshold) (integrate-kepler ...)]
    [else
     (... (fn-integrate-dc ...)
          (fn-integrate-dc ...))]))

(define (integrate-dc f a b)
  (cond
    [(<= (- b a) threshold) (integrate-kepler f a b)]
    [else
     (+ (integrate-dc f a (+ a (/ (- b a) 2)))
        (integrate-dc f (+ a (/ (- b a) 2)) b))]))








