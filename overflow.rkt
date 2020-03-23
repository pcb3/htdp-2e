;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname overflow) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Overflow

;;====
;; 415

; Number -> Number
; consumes a number n and produces a number such that when an
; n is raised to (i + 1) it produces +inf.0
(check-satisfied (expt #i10.0 (raised-to #i10.0 0))
                 (lambda (x)
                   (and (inexact? x) (not (equal? #i+inf.0 x)))))
(check-satisfied (expt #i10.0 (add1 (raised-to #i10.0 0)))
                 (lambda (x) (equal? #i+inf.0 x)))
              
(define (fn-raised-to n i)
  (cond
    [(= (expt ... ...) ...) (sub1 ...)]
    [else (fn-raised-to ... (add1 ...))]))

(define (raised-to n i)
  (cond
    [(= (expt n i) +inf.0) (sub1 i)]
    [else (raised-to n (add1 i))]))

;;=========================
;; Underflow

;; 416

(check-satisfied
 (expt #i10.0 (add1 (raised-to-abstract #i10.0 0 +inf.0)))
 (lambda (x) (equal? #i+inf.0 x)))

(check-satisfied
 (expt #i10.0 (sub1 (raised-to-abstract #i10.0 0 #i0.0)))
 (lambda (x) (equal? #i0.0 x)))

(define (fn-raised-to-abstract n i goal)
  (cond
    [(< ... ...)
     (cond
       [(= (expt ... ...) goal) (add1 ...)]
       [else (fn-raised-to-abstract ... (sub1 ...) ...)])]
    [else
     (cond
       [(= (expt ... ...) goal) (sub1 ...)]
       [else (fn-raised-to-abstract ... (add1 ...) ...)])]))

(define (raised-to-abstract n i goal)
  (local
    
    ((define less-than? (< goal n)))
    
    (cond
      [(= (expt n i) goal)
       (cond [else (if less-than? (add1 i) (sub1 i))])]
      [else
       (raised-to-abstract
        n (cond [else (if less-than? (sub1 i) (add1 i))])
        goal)])))













        
