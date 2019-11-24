;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname data-analysis) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 20 Iterative Refinement

; 20.1 Data Analysis

; A file is one of:
; - Atom
; - (cons Atom File)

; 20.2 Refining Data Definitions

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.

; Exercise 330

(define CODE '("hang" "draw"))
(define DOCS '("read"))
(define TEXT '("part1" "part2" "part3"))
(define LIBS (list CODE DOCS))
(define TS (list TEXT "read!" LIBS))

; Exercise 331

; Dir.v1 -> Number
; consumes a Dir.v1 dir and produces the number of
; files in the given directory

(check-expect (how-many '()) 0)
(check-expect (how-many CODE) 2)
(check-expect (how-many TS) 7)

(define (fn-how-many dir)
  (cond
    [(empty? dir) ...]
    [(list? (first dir))
     (... (fn-how-many (first dir))
          (fn-how-many (rest dir)))]
    [(string? (first dir))
     (... (fn-how-many (rest dir)))]
    [else
     (fn-how-many (rest dir))]))

(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(list? (first dir))
     (+ (how-many (first dir))
        (how-many (rest dir)))]
    [(string? (first dir))
     (add1 (how-many (rest dir)))]
    [else
     (how-many (rest dir))]))

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

; Exercise 332

(define DIR2
  (cons (make-dir "TS"
                  (cons
                   (make-dir "Text"
                             (cons "part1"
                                   (cons "part2"
                                         (cons "part3"
                                               '()))))
                   (cons "read!"
                         (cons
                          (make-dir "Libs"
                                    (cons (make-dir "Code"
                                                    (cons "hang"
                                                          (cons "draw" '())))
                                          (cons
                                           (make-dir "Docs"
                                                     (cons "read!" '())) '())))
                          '())))) '()))
  
; Exercise 333      

; Dir.v2 -> Number
; consumes a directory and determines the number of
; files it contains

(check-expect (how-many-v2 DIR2) 7)

(define (fn-how-many-v2 dir)
  (cond
    [(empty? dir) ...]
    [(dir? (first dir))
     (fn-how-many-v2 (dir-content (first dir)))]
    [(string? (first dir))
     (... (fn-how-many-v2 (rest dir)))]
    [(list? (first dir))
     (... (fn-how-many-v2 (first dir))
          (fn-how-many-v2 (rest dir)))]
    [else
     (fn-how-many-v2 (rest dir))]))
     
    
(define (how-many-v2 dir)
  (cond
    [(empty? dir) 0]
    [(dir? (first dir))
     (+ (how-many-v2 (dir-content (first dir)))
        (how-many-v2 (rest dir)))]
    [(string? (first dir))
     (add1 (how-many-v2 (rest dir)))]
    [(list? (first dir))
     (+ (how-many-v2 (first dir))
        (how-many-v2 (rest dir)))]
    [else
     (how-many-v2 (rest dir))]))

; Exercise 334

(define-struct dir.v2 [name content size read?])

; A dir-v2 is a structure:
; (make-dir-v2 String LOFD Number String)
; The size field is the amount of space consumed
; by the directory. read? determines the rights of
; use

(define DIR.V2-1
  (make-dir.v2 "Test" '() 10 "root"))

;; Model 3

(define-struct file [name size content])

; A File.v3 is a structure:
; (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; Exercise 335
(define DIR.V3
  (cons
   (make-dir.v3 "TS"
                (cons
                 (make-dir.v3 "Text" '()
                              (cons
                               (make-file "part1" 99 "")
                               (cons
                                (make-file "part2" 52 "")
                                (cons
                                 (make-file "part3" 17 "")
                                 '()))))
                 (cons
                  (make-dir.v3 "Libs"
                               (cons
                                (make-dir.v3 "Code" '()
                                             (cons
                                              (make-file "hang" 8 "")
                                              (cons
                                               (make-file "draw" 2 "")
                                               '())))
                                (cons
                                 (make-dir.v3 "Docs" '()
                                              (cons
                                               (make-file "read!" 19 "")
                                               '()))
                                 '())) '())
                  '()))
                (cons
                 (make-file "read!" 10 "")
                 '()))
   '()))
           
; Exercise 336

; Dir.v3 -> Number
; consumes a directory dir and produces the number
; of files

(check-expect (how-many-v3 DIR.V3) 7)

(define (fn-how-many-v3 dir)
  (cond
    [(empty? dir) ...]
    [(dir.v3? (first dir))
     (... (... (... (dir.v3-files dir))
               (fn-how-many-v3 (dir.v3-dirs dir)))
          (fn-how-many-v3 (rest dir)))]
    [else
     (fn-how-many-v3 (rest dir))]))

(define (how-many-v3 dir)
  (cond
    [(empty? dir) 0]
    [(dir.v3? (first dir))
     (+ (+ (length (dir.v3-files (first dir)))
           (how-many-v3 (dir.v3-dirs (first dir))))
        (how-many-v3 (rest dir)))]
    [else
     (how-many-v3 (rest dir))]))





































    
