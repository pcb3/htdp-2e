;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname designing-algorithms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 26 Designing Algorithms

;; 26.1 Adapting the Design Recipe

;;====
;; 431

;== bundle problem

; trivial example: when the list is empty, the chunk size is >=
; length of the list

; solve trivial case by returning the original list

; we generate several new problems. Each new problem is smaller
; by n. The newly generated is simply combining n items from a
; list of items and removing the first n items from the list

; we need to combine the solutions of the generated problems. We
; don't need anything from the original data.

; 0 if s '() or n >= length of s.
; integer division of s by n or
; integer dvision of s by n then add 1 to the result

;== quick-sort< problem

; What is a trivially solvable problem?

; trivail problem is the empty list and when there is only one

; How are trivial solutions solved?
; item in the list

; trivial problem solved by returning the list

;How does the algorithm generate new problems that are more
;easily solvable than the original one? Is there one new problem
;that we generate or are there several?

; The algorithm partitions the list into a pivot and a list
; smaller and larger relative to the pivot. The lists are then
; solved recursively.

;;====
;; 432

(define MAX 100)

; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

; Posn -> Posn
; ???
; consumes a Posn p and produces a Posn different from p
; termination: when the posn created is not equal to
; the candidate else loops.
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)

(define (food-create p)
  (local
    ((define (food-check-create pos candidate)
       (if (equal? pos candidate) (food-create p) candidate)))
    (food-check-create
     p (make-posn (random MAX) (random MAX)))))

;;