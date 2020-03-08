;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname db) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 23.7 Project: Database

; This section pulls together knowledge from all
; four parts of the book.

(require 2htdp/abstraction)

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)

;;====
;; 403

(define school-schema
  `(,(make-spec "Name" string?)
    ,(make-spec "Age" integer?)
    ,(make-spec "Present" boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  `((#true "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema presence-content))

;;====
;; 404

(define SCHEMA0 `(,(make-spec "Name" string?)
                  ,(make-spec "Age" number?)
                  ,(make-spec "Infected" boolean?)))
(define CONTENT0 '("Harry" 12 #false))

(define (integrity a b)
  ((spec-predicate a) b))

; [X Y] [X Y -> Boolean]
; [List-of X] [List-of Y] -> Boolean
; consumes a function f a Schema sch and Row row
; and produces true if f produces true for pairs of
; corresponding values on the two lists

(check-expect
 (andmap2 integrity SCHEMA0 CONTENT0) #true)

(define (andmap2 f sch row)
  (cond
    [(empty? sch) #true]
    [else (and (f (first sch) (first row))
               (andmap2 f (rest sch) (rest row)))]))

;;====
;; 405

; Row [List-of Label] -> Row
; retains those cells whose corresponding element 
; in names is also in labels
(check-expect
 (row-filter '("Alice" 35 #true)
             '("Name" "Age" "Present"))
 '("Alice" #true))

(define (fn-row-filter row names)
  (cond
    [(empty? row) ...]
    [else
     (... (member? (first names) '("Name" "Present"))
          (... (first row)
               (fn-row-filter (rest row)
                              (rest names)))
     (fn-row-filter (rest row) (rest names)))]))

(define (row-filter row names)
  (cond
    [(empty? names) '()]
    [else
     (if (member? (first names) '("Name" "Present"))
         (cons (first row)
               (row-filter (rest row)
                           (rest names)))
         (row-filter (rest row) (rest names)))]))

;;====
;; 406

(define (project.v1 db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
          (define labels (map first schema))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (row-filter row labels))
 
          ; Row [List-of Label] -> Row
          ; retains those cells whose name is in labels
          (define (row-filter row names)
            (cond
              [(empty? names) '()]
              [else
               (if (member? (first names) labels)
                   (cons (first row)
                         (row-filter (rest row) (rest names)))
                   (row-filter (rest row) (rest names)))])))
    (make-db (filter keep? schema)
             (map row-project content))))

;;====
;; 407

(define LABELS '("Name" "Present"))

; Row [List-of Label] -> Row
; retains those cells whose name is in labels using
; foldr
(check-expect
 (row-filter-foldr '("Alice" 35 #true)
                   '("Name" "Age" "Present"))
 '("Alice" #true))

(define (row-filter-foldr row names)
  (foldr (lambda (a b c) (if (member? b LABELS)
                           (cons a c)
                           (append '() c)))
         '() row names))















