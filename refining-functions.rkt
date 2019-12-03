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
           
; A Path is [List-of String].
; interpretation directions into a directory tree

(define PATH-TO-PART1 (list "TS" "Text" "part1"))

; Exercise 342

; Dir* File* -> Path
; consumes a directory d and a file f and produces
; the path to the file if find? f is #true or
; false otherwise

(check-expect (find test "thisone.txt")
              (list
               '/home/pc/code/test
               '/home/pc/code/test/second
               "thisone.txt"))

(check-expect (find test "test.txt")
              (list
               '/home/pc/code/test
               '/home/pc/code/test/first
               "test.txt"))

(check-expect (find test "umm.txt") #false)

(define (fn-find d f)
  (local
    (; Dir* -> Dir*
     ; consumes a Dir* d and produces the directory
     ; containing the file or the next step in the
     ; path
     (define (fn-dl-process di)
       (cond
         [(empty? di) ...]
         [(find? (first di) f)
          (fn-d-process (first di))]
         [else
          (fn-dl-process (rest di))]))

     ; Dir* -> Dir*
     ; consumes a Dir* and produces the name of that
     ; directory if it contains the file f
     (define (fn-d-process di)
       (cond
         [(find? di f)
          (cond
            [(fn-contains-f? di)
             (... (dir-name di))]
            [else
             (fn-find (dir-dirs di) f)])]))

     ; Dir* -> Boolean
     ; consumes a directory di and produces true if
     ; it contains the file f
     (define (fn-contains-f? d)
       (member? f (dir-files d))))
  
    (cond
      [(dir? d) (fn-d-process d)]
      [else
       (fn-dl-process d)])))

(define (find d f)
  (local
    (; Dir* -> Dir*
     ; consumes a Dir* d and produces the directory
     ; containing the file or the next step in the
     ; path
     (define (dl-process di)
       (cond
         [(find? (first di) f)
          (d-process (first di))]
         [else
          (dl-process (rest di))]))

     ; Dir* -> Dir*
     ; consumes a Dir* and produces the name of that
     ; directory if it contains the file f
     (define (d-process di)
       (cond
         [(find? di f)
          (cond
            [(contains-f? di)
             (cons (dir-name di) (list f))]
            [else
             (cons (dir-name di)
                   (dl-process (dir-dirs di)))])]))

     ; Dir* -> Boolean
     ; consumes a directory di and produces true if
     ; it contains the file f
     (define (contains-f? d)
       (member? f
                (map (lambda (t) (file-name t))
                     (dir-files d)))))
    
    (cond
      [(find? d f)
       (d-process d)]
      [else #false])))
       
       
                


























