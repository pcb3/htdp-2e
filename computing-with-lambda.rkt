;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname computing-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 17.2 Computing with lambda

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
    (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

;(compare (random 100000))

((lambda (x) (* 10 x)) ((lambda (x) (* 10 x)) 42))

 exercise 283

(define th 20)
(define-struct ir [name price])

((lambda (ir) (<= (ir-price ir) th))
 (make-ir "bear" 10))

(filter (lambda (ir) (<= (ir-price ir) th))
        (list (make-ir "bear" 10)
              (make-ir "snake" 99)))

(map (lambda (x) (* 10 x))
     '(1 2 3))

(foldl (lambda (name rst)
         (string-append name ", " rst))
       "etc."
       '("Mathew" "Robby"))

 exercise 284

((lambda (x) x) ((lambda (x) x) 1))

((lambda (x) (x x)) (lambda (x) x))

((lambda (x) (x x)) (lambda (x) (x x)))
















         