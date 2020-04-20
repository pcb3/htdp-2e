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
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
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
             (map (row-filter content labels)))))

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
                             (cons a c) c))
         '() row names))


;;====
;; 408

(define DB0
  (make-db `(("Name" string?)
             ("Age" number?)
             ("Present" boolean?))
           '(("Alice" 35 #true)
             ("Bob"   25 #false)
             ("Woody" 13 '@))))

(check-expect
 (project DB0 '("Age"))
 (make-db `(("Age" number?))
          '((35) (25) (13))))
 

(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          
          (define (keep? c) (member? (first c) labels))
          
          (define mask (map keep? schema))

          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '() row mask)))
          
    (make-db (filter keep? schema)
             (map row-project content))))

; DB [List-of Label] [Row -> Boolean] -> DB
; consumes a database, a list of labels and a predicate
; for each row that satisfies the given predicate
; projected down to the given set of labels
(check-expect
 (select DB0
         '("Age")
         `(,string? ,number? ,boolean?))
 (make-db `(("Age" number?))
          '((35) (25))))

(define (fn-select db labels predicate)
  (local
    ((define (row-satisfies? row pred)
       (cond
         [(empty? row) ...]
         [((first pred) (first row))
          (row-satisfies? (rest row)
                          (rest pred))]
         [else ...])))
    (project
     (make-db (db-schema db)
              (map (lambda (r)
                     (row-satisfies? ... ...))
                   (db-content db)))
     ...)))

(define (select db labels predicate)
  (local
    
    ((define schema (db-schema db))
     (define content (db-content db))

     (define (pred-satisfy row)
       (andmap (lambda (x)
                 (boolean=? #true x))
               (foldr (lambda (r p b)
                        (if (p r) (cons #t b) (cons #f b)))
                      '() row predicate))))
    
    (project
     (make-db schema (filter pred-satisfy content))
     labels)))

;;====
;; 409

; DB [List-of Label] -> DB
; consumes a database db and a list of labels lol and
; produces a new db with the columns reordered according
; to lol

(check-expect
 (reorder (make-db `(("Name" string?)
                     ("Health" number?)
                     ("Mana" number?)
                     ("Hardcore" boolean?))
                   '(("Fern" 108 200 #true)
                     ("Pus" 120 220 #true)
                     ("Ophinian" 400 90 #false)))
          '("Name" "Health" "Mana" "Hardcore"))
 (make-db `(("Name" string?)
            ("Health" number?)
            ("Mana" number?)
            ("Hardcore" boolean?))
          '(("Fern" 108 200 #true)
            ("Pus" 120 220 #true)
            ("Ophinian" 400 90 #false))))

(check-expect
 (reorder (make-db `(("Name" string?)
                     ("Health" number?)
                     ("Mana" number?)
                     ("Hardcore" boolean?))
                   '(("Fern" 108 200 #true)
                     ("Pus" 120 220 #true)
                     ("Ophinian" 400 90 #false)))
          '("Health" "Mana" "Hardcore""Name" ))
 (make-db `(("Health" number?)
            ("Mana" number?)
            ("Hardcore" boolean?)
            ("Name" string?))
          '((108 200 #true "Fern")
            (120 220 #true "Pus")
            (400 90 #false "Ophinian"))))

(define (fn-reorder db lol)
  (local
    ((define schema (db-schema db))
     (define content (db-content db))

     (define (position start label schm)
       (cond
         [(empty? schm) ...]
         [else
          (... (equal? ... (first (first schm)))
               ...
               (positon (add1 ...) ... (rest schm)))]))

     (define position-list
       (map (lambda (lbl) (position ... lbl schema)) lol)))

    (make-db (map (lambda (n)
                    (list-ref ... ...)) position-list)
             (map
              (lambda (row)
                (map (lambda (n)
                       (list-ref ... ...)) position-list))
              ...))))
             
(define (reorder db lol)
  (local

    ((define schema (db-schema db))
     (define content (db-content db))

     (define (position start label schm)
       (cond
         [(empty? schm) '()]
         [else
          (if
           (equal? label (first (first schm)))
           start
           (position (add1 start) label (rest schm)))]))

     (define position-list
       (map (lambda (lbl) (position 0 lbl schema)) lol)))

    (make-db (map (lambda (n)
                    (list-ref schema n)) position-list)
             (map
              (lambda (row)
                (map (lambda (n)
                       (list-ref row n)) position-list))
              content))))
         
;;====
;; 410

; DB DB -> DB
; consumes two DB's with the same schema and produces a new
; DB that is the union of boths content
(check-expect
 (db-union (make-db `(("Name" string?)
                      ("Local" boolean?)
                      ("Price" number?))
                    `(("Apple" #true 3)
                      ("Orange" #true 4)
                      ("Banana" #false 1)))
           (make-db `(("Name" string?)
                      ("Local" boolean?)
                      ("Price" number?))
                    `(("Pear" #true 5)
                      ("Date" #false 20)
                      ("Banana" #false 1))))
 (make-db `(("Name" string?)
            ("Local" boolean?)
            ("Price" number?))
          `(("Apple" #true 3)
            ("Orange" #true 4)
            ("Banana" #false 1)
            ("Pear" #true 5)
            ("Date" #false 20))))

(define (db-union db1 db2)
  (local
    (
     
     (define schema-db1 (db-schema db1))
     (define content-db1 (db-content db1))
     
     (define (exclusive-list db)
       (cond
         [(empty? (db-content db)) '()]
         [else
          (if (member? (first (db-content db))
                       content-db1)
              (exclusive-list
               (make-db schema-db1
                        (rest (db-content db))))
              (cons (first (db-content db))
                    (exclusive-list
                     (make-db schema-db1
                              (rest (db-content db))))))])))
    (make-db schema-db1
             (append
              content-db1
              (exclusive-list db2)))))

;;====
;; 411

; DB DB ->
; consumes two DB's and joins them together by replacing
; the last cell in each row of the first DB with the
; translation of the cell in the second DB
(check-expect
 (join (make-db `(("Name" string?)
                  ("Price" number?)
                  ("Local" boolean?))
                `(("Apple" 3 #true)
                  ("Orange" 4 #true)
                  ("Banana" 1 #false)))
       (make-db `(("Local" boolean?)
                  ("Description" string?))
                `((#true "New Zealand")
                  (#false "Imported"))))
 (make-db `(("Name" string?)
            ("Price" number?)
            ("Description" string?))
          `(("Apple" 3 "New Zealand")
            ("Orange" 4 "New Zealand")
            ("Banana" 1 "Imported"))))

(check-expect
 (join (make-db `(("Name" string?)
                  ("Price" number?)
                  ("Local" boolean?))
                `(("Apple" 3 #true)
                  ("Orange" 4 #true)
                  ("Banana" 1 #false)))
       (make-db `(("Local" boolean?)
                  ("Description" string?))
                `((#true "New Zealand")
                  (#true "Seasonal")
                  (#false "Imported")
                  (#false "Non-seasonal"))))
 (make-db `(("Name" string?)
            ("Price" number?)
            ("Description" string?))
          `(("Apple" 3 "New Zealand")
            ("Apple" 3 "Seasonal")
            ("Orange" 4 "New Zealand")
            ("Orange" 4 "Seasonal")
            ("Banana" 1 "Imported")
            ("Banana" 1 "Non-seasonal"))))
   
(define (join db1 db2)
  (local

    ((define content1 (db-content db1))
     (define content2 (db-content db2))
     (define schema1 (db-schema db1))
     (define schema2 (db-schema db2))

     (define (build-content con1)
       (if (empty? con1)
           '()
           (append (build-row (first con1) content2)
                   (build-content (rest con1)))))

     (define (build-row row content)
       (cond
         [(empty? content) '()]
         [else
          (if (equal? (first (reverse row))
                      (first (first content)))
              (cons (translate row (second (first content)))
                    (build-row row (rest content)))
              (build-row row (rest content)))]))

     (define (translate r cell)
       (reverse (cons cell (rest (reverse r))))))
    
    (make-db
     (map (lambda (schm-row)
            (if (equal? schm-row (first schema2))
                (second schema2) schm-row))
          schema1)
     (build-content content1))))

;;