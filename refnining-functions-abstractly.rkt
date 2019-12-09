;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refnining-functions-abstractly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

(define test (create-dir "/home/pc/code/test"))
(define test2 (create-dir "/home/pc/code/test2"))
(define test3 (create-dir "/home/pc/code/test3"))
(define test4 (create-dir "/home/pc/code/test4"))

; Dir String -> Boolean
; consumes a Dir dir and a filename f and returns
; true if the file occurs in the given dir

(check-expect (find?-abstract test "hello.py") #false)
(check-expect (find?-abstract test "thisone.txt") #true)

(define (find?-abstract dir f)
  (cond
    [(dir? dir) (or (ormap (lambda (p) (string=? f (file-name p)))
                           (dir-files dir))
                    (find?-abstract (dir-dirs dir) f))]
    [else
     (ormap (lambda (q) (find?-abstract q f)) dir)]))
           

; Dir -> Path
; consumes a Dir dir and produces the names
; of all files and directories

(check-expect (andmap
               (lambda (a)
                 (member? a
                          (list
                           '/home/pc/code/test
                           '/home/pc/code/test/first
                           '/home/pc/code/test/second
                           "happy.txt" "test.txt" "thisone.txt")))
               (ls-abstraction test)) #true)
               
(define (ls-abstraction dir)
  (append (list (dir-name dir))
          (map (lambda (x) (file-name x)) (dir-files dir))
          (foldl (lambda (x y) (append (ls-abstraction x) y))
                 '() (dir-dirs dir))))

;; aleternate method

(define (ls-abstraction1 dir)
  (local
    ((define (dir-file-names1 d)
       (append 
        (map (lambda (x) (file-name x)) (dir-files d))
        (foldl (lambda (x y) (append (ls-abstraction1 x) y))
               '() (dir-dirs d)))))
    (cons (dir-name dir) (dir-file-names1 dir))))

; Dir* -> Number
; consumes a Dir* dir and produces the total size of
; all files, assuming cost 1 per directory

; List-of File* -> Number
; consumes a File* f and produces the total size
; of all the files

(define FILE-SIZE 29629947)
(define SIZE-TEST2 (+ FILE-SIZE 3))

(check-expect (du-abstract test2) SIZE-TEST2)

(define (du-abstract dir)
  (foldl (lambda (x y) (+ (du-abstract x) y))
         (foldl + 1 (map (lambda (s) (file-size s)) (dir-files dir)))
         (dir-dirs dir)))

; Dir* File* -> Path
; consumes a directory d and a file f and produces
; the path to the file if find? f is #true or
; false otherwise

;(check-expect (find-abstract test "thisone.txt")
;              (list
;               '/home/pc/code/test
;               '/home/pc/code/test/second
;               "thisone.txt"))
;
;(check-expect (find-abstract test "test.txt")
;              (list
;               '/home/pc/code/test
;               '/home/pc/code/test/first
;               "test.txt"))

(check-expect (find-abstract test "umm.txt") #false)   

(define (find-abstract dir f) #false)
    





























