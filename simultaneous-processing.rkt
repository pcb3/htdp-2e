;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simultaneous-processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;===========================
;; 23 Simultaneous Processing

;;================================================
;; 23.1 Processing Two Lists Simultaneously: Case1

;;==============
;; 387

; [List-of Symbol] [List-of Number] -> [List-of List]
; consumes a list of Symbols los and a list of Numbers
; lon and produces all possible oredered pairs
(check-expect (cross '(a) '(1 2 3)) '((a 1) (a 2) (a 3)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b) '(1 2))
              '((a 1) (a 2) (b 1) (b 2)))

(define (fn-cross los lon)
  (cond
    [(empty? los) ...]
    [else
     (... (... (first los) lon)
          (fn-cross (rest los) lon))]))
    
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else
     (append
      (map (lambda (p) (cons (first los) (list p))) lon)
      (cross (rest los) lon))]))

;;=================================================
;; 23.2 Processing two lists simultaneously: Case 2

;;====
;; 388

;;=========================
;; structures

(define-struct employee [name ssn payrate])
; an employee is a structure:
; (make-employee (String Number Number)
; Interpretation: employee is a record including their
; pay rate in $USD per hour

(define EMPLOYEE0 (make-employee "freddy" 123 25))
(define EMPLOYEE1 (make-employee "jason" 888 4))

(define-struct wr [name hours])
; a wr (work record) is a structure:
; (make-wr (String Number))
; Interpretation: a wr is a record of the number of hours
; worked per week of an employee.

(define WR0 (make-wr "freddy" 120))
(define WR1 (make-wr "jason" 40))

;;==========================

; [List-of employee] [List-of wr] -> [List-of Number]
; multiplies the payrate field in employee structure
; with the hours field in wr to find weekly $USD wages.
; assume the two lists are of equal length 

(check-expect (wages* '() '()) '())
(check-expect (wages* `(,EMPLOYEE0) `(, WR0))
              `(,(* (employee-payrate EMPLOYEE0)
                    (wr-hours WR0))))
(check-expect (wages* `(,EMPLOYEE0 ,EMPLOYEE1)
                      `(,WR0 ,WR1))
              `(,(* (employee-payrate EMPLOYEE0)
                    (wr-hours WR0))
                ,(* (employee-payrate EMPLOYEE1)
                    (wr-hours WR1))))

(define (wages* loe low)
  (cond
    [(empty? loe) '()]
    [else
     (cons
      (weekly-wage (employee-payrate (first loe))
                   (wr-hours (first low)))
      (wages* (rest loe) (rest low)))]))

; employee wr -> Number
; computes the weekly wage from pay-rate and hours fields
; from employee and wr structures
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

;;==========================
;; 389

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

(define PR0 (make-phone-record "julia" "123"))
(define PR1 (make-phone-record "dan" "456"))

; A Lopr (list of phone records) is one of:
; - '()
; - (cons phone-record Lopr)

(define LOPR0 `(,PR0 ,PR1))

; [List-of String] [List-of String] -> Lopr
; consumes a list of names lon as Strings and a list
; of String as phone numbers lop and produces a Lopr
(check-expect (zip '() '()) '())

(check-expect (zip `("julia" "dan") `("123" "456"))
              LOPR0)

(define (fn-zip lon lop)
  (cond
    [(empty? lon) ...]
    [else
     (cons
      (... (first lon) (first lop))
      (fn-zip (rest lon) (rest lop)))]))

(define (zip lon lop)
  (cond
    [(empty? lon) '()]
    [else
     (cons
      (make-phone-record (first lon) (first lop))
      (zip (rest lon) (rest lop)))]))




















