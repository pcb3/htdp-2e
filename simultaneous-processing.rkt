;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simultaneous-processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;===========================
;; 23 Simultaneous Processing

;;================================================
;; 23.1 Processing pwo lists simultaneously: Case 1

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

(define EMPLOYEE0 (make-employee "freddy" 123 75))
(define EMPLOYEE1 (make-employee "jason" 888 4))

(define-struct wr [name hours])
; a wr (work record) is a structure:
; (make-wr (String Number))
; Interpretation: a wr is a record of the number of hours
; worked per week of an employee.

(define WR0 (make-wr "freddy" 32))
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

;;=================================================
;; 23.3 Processing two lists simultaneously: Case 3

;;==========
;; 390 & 392

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)

(define TOS0 '*)
(define TOS1 (make-branch '^ '&))
(define TOS2 (make-branch (make-branch '! '@)
                          (make-branch '$ '%)))


; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 
; - '()
; (cons Direction Path)

; Path TOS -> Symbol
; consumes a Path p and a Tree of Symbols tos and
; produces the symbol at the end of the path or signals
; an error

(check-expect (tree-pick '() '*) '*)
(check-expect (tree-pick '(left) TOS1) '^)
(check-expect (tree-pick '(right left) TOS2) '$)
(check-expect (tree-pick '() TOS1) TOS1)
(check-expect (tree-pick '(right) '$) "end of branch")

(define (fn-tree-pick p tos)
  (cond
    [(and (empty? p) (symbol? tos)) ...]
    [(and (empty? p) (branch? tos)) ...]
    [(and (cons? p) (symbol? tos)) ...]
    [(and (cons? p) (branch? tos))
     (... (rest p)
          (cond (else (if (symbol=? ... (first p))
                          (... ...)))))]))

(define (tree-pick p tos)
  (cond
    [(and (or (branch? tos) (symbol? tos))
          (empty? p)) tos]
    [(and (or (branch? tos) (symbol? tos)) 
          (cons? p)) "end of branch"]
    (tree-pick
     (rest p)
     (cond (else (if (symbol=? 'right (first p))
                     (branch-right tos)
                     (branch-left tos)))))]))

;;===========================
;; 391

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '(a b)) '(a b))
(check-expect (replace-eol-with '(1 2) '())
              (cons 1 (cons 2 '())))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with
               (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (fn-replace-eol-with front end)
  (cond
    [(and (empty? front) (empty? end)) ...]
    [(and (empty? front) (cons? end)) ...]
    [(and (cons? front) (empty? end)) ...]
    [(and (cons? front) (cons? end))
     (... ... (... (rest front) ...))]))

(define (replace-eol-with front end)
  (cond
    [(empty? front) end]
    [(empty? end) front]
    [(cons? front)
     (cons (first front)
           (replace-eol-with (rest front) end))]))
    









