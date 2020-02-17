;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname complex-inputs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================
;; 23.5 Designing functions that
;;      consume two complex inputs

;;==================================
;; 23.6 Finger exercises: two inputs

;;====
;; 393
	
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define SON.R0 '())
(define SON.R1 '(1 3 5))

; SON.R SON.R -> SON.R
; consumes two sets s1 s2 and produces the union
(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(1)) '(1))
(check-expect (sort (union '(1 3 5) '(2 4 6)) <)
              '(1 2 3 4 5 6))

(define (fn-union s1 s2)
  (cond
    [(empty? s1) ...]
    [else
     (if (member? (first s1) ...)
         (fn-union (rest s1) ...)
         (cons (first s1) (fn-union (rest s1) ...)))]))

(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [else
     (if (member? (first s1) s2)
         (union (rest s1) s2)
         (cons (first s1) (union (rest s1) s2)))]))

; SON.R SON.R -> SON.R
; consumes two sets s1 s2 and produces their intersection
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '() '(1)) '())
(check-expect (intersect '(1) '(2)) '())
(check-expect (intersect '(1 2) '(2 3)) '(2))

(define (fn-intersect s1 s2)
  (cond
    [(empty? s1) ...]
    [else (if (member? (first s1) ...)
              (cons (first s1)
                    (fn-intersect (rest s1) ...))
              (fn-intersect (rest s1) ...))]))

(define (intersect s1 s2)
  (cond
    [(empty? s1) '()]
    [else (if (member? (first s1) s2)
              (cons (first s1)
                    (intersect (rest s1) s2))
              (intersect (rest s1) s2))]))


























