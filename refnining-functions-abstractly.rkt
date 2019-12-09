;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refnining-functions-abstractly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

(define test (create-dir "/home/pc/code/test"))
(define test2 (create-dir "/home/pc/code/test2"))
(define test3 (create-dir "/home/pc/code/test3"))
(define test4 (create-dir "/home/pc/code/test4"))

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
  (local
    ((define (dir-file-names d)
       (append (list (dir-name d))
               (map (lambda (x) (file-name x)) (dir-files d))
               (foldl (lambda (x y) (append (dir-file-names x) y))
                      '() (dir-dirs d)))))
    (dir-file-names dir)))

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
  (local
    ((define (add-files d)
       (+ 1
          (foldl + 0 (map (lambda (s) (file-size s)) (dir-files d)))
          (foldl (lambda (x y) (+ (du-abstract x) y))
                 0 (dir-dirs d)))))

    (add-files dir)))

    
           
    





























