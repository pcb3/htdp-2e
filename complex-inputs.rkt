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

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  
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

;;=======================
;; 394

; SON.L SON.L -> SON.L
; consumes two sets s1 s2 and produces the union of
; both including repeats
(check-expect (merge '() '()) '())
(check-expect (merge '() '(1)) '(1))
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (merge '(1 1 2) '(3 3 4)) '(1 1 2 3 3 4))             

(define (fn-merge s1 s2)
  (cond
    [(and (cons? ...) (cons? ...))
     (...
      (first s1)
      (... (first s2) (fn-merge (rest s1) (rest s2))))]
    [(and (cons? ...) (empty? ...))
     (... (first s1) (fn-merge (rest s1) ...))]
    [(and (empty? ...) (cons? ...))
     (... (first s2) (fn-merge ... (rest s2)))]
    [(and (empty? ...) (empty? ...))
     ...]))

(define (merge s1 s2)
  (cond
    [(and (cons? s1) (cons? s2))
     (cond
       [else (if (<= (first s1) (first s2))
                 (cons
                  (first s1)
                  (merge (rest s1) s2))
                 (cons (first s2) (merge s1) (rest s2)))])]
    [(and (cons? s1) (empty? s2))
     (cons (first s1) (merge (rest s1) s2))]
    [(and (empty? s1) (cons? s2))
     (cons (first s2) (merge s1 (rest s2)))]
    [(and (empty? s1) (empty? s2)) '()]))
















































