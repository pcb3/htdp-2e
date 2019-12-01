;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 20.3 Refining Functions

(require 2htdp/abstraction)
(require htdp/dir)

; Exercise 338

(define test (create-dir "/home/pc/code/test"))
(define test2 (create-dir "/home/pc/code/test2"))
(define working-dir (create-dir "/home/pc/code/htdp"))

; exercise 339

; Dir String -> Boolean
; consumes a Dir dir and a filename f and returns
; true if the file occurs in the given dir

(check-expect (find? working-dir "itunes.rkt") #true)
(check-expect (find? working-dir "hello.py") #false)
(check-expect (find? working-dir "hi.py") #true)
(check-expect (find? test "thisone.txt") #true)

(define (find? dir f)
  (cond
    [(empty? dir) #false]
    [(dir? dir)
     (or (ormap-files dir f)
         (check-dir-list (dir-dirs dir) f))]
    [(list? dir)
     (check-dir-list dir f)]
    [else
     #false]))
    
(define (check-dir-list l f)
  (cond
    [(empty? l) #false]
    [else (or (find? (first l) f)
              (find? (rest l) f))]))

(define (ormap-files d f)
  (ormap
   (lambda (x)
     (string=? (file-name x) f)) (dir-files d)))

; Exercise 340

; Dir -> List-of Strings
; consumes a Dir dir and produces the names of all
; files and directories in the given Dir                                

(check-expect (andmap
               (lambda (a)
                 (member? a
                          (list
                           '/home/pc/code/test
                           '/home/pc/code/test/first
                           '/home/pc/code/test/second
                           "happy.txt" "test.txt" "thisone.txt")))
               (ls-empty? test)) #true)
        
(define (ls-empty? dir)
  (cond
    [(empty? dir) '()]
    [else
     (cons (dir-name dir)
           (append
            (map (lambda (f) (file-name f)) (dir-files dir))
            (extract-dir (dir-dirs dir))))]))

(define (extract-dir d)
  (cond
    [(empty? d) '()]
    [else (append (ls-empty? (first d))
                  (extract-dir (rest d)))]))

; Exercise 341

; Dir* -> Number
; consumes a Dir* dir and produces the total size of
; all files, assuming cost 1 per directory

(define FILE-SIZE 29629947)
(define SIZE-TEST2 (+ FILE-SIZE 3))

(check-expect (du test2) SIZE-TEST2)

(define (fn-du dir)
  (cond
    [(empty? dir) ...]
    [(list? dir)
     (... (process-files (dir-files (first dir)))
          (fn-du (dir-dirs dir))
          (fn-du (rest dir)))]
    [else
     (... (process-files (dir-files dir))
          (fn-du (dir-dirs dir)))]))

; List-of File* -> Number
; consumes a File* f and produces the total size
; of all the files

(define (fn-process-files lf)
  (cond
    [(empty? lf) ...]
    [else
     (... (... (file-size (first lf))
               (fn-process-files (rest lf))))]))

(define (du dir)
  (cond
    [(empty? dir) 0]
    [(list? dir)
     (+ (process-files (dir-files (first dir)))
        (du (dir-dirs (first dir)))
        (du (rest dir)))]
    [else
     (+ (process-files (dir-files dir))
        (du (dir-dirs dir)))]))

(define (process-files lf)
  (cond
    [(empty? lf) 0]
    [else
     (add1 (+ (file-size (first lf))
              (process-files (rest lf))))]))
           
    
                


























