;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refnining-functions-abstractly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

(define test (create-dir "/home/pc/code/test"))
(define test2 (create-dir "/home/pc/code/test2"))
(define test3 (create-dir "/home/pc/code/test3"))
(define test4 (create-dir "/home/pc/code/test4"))
(define test5 (create-dir "/home/pc/code/test5"))
(define test6 (create-dir "/home/pc/code/test6"))

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

(check-expect (find-abstract test "thisone.txt")
              (list
               '/home/pc/code/test
               '/home/pc/code/test/second
               "thisone.txt"))

(check-expect (find-abstract test "test.txt")
              (list
               '/home/pc/code/test
               '/home/pc/code/test/first
               "test.txt"))

(check-expect (find-abstract test "umm.txt") #false)   

(define (fn-find-abstract dir f)
  (local
    (; Dir -> Path
     ; consumes a list of Dir lod and produces the path
     (define (fn-dl-process lod)
       (cond
         [(in-file? (first lod)) (... (dir-name (first lod))
                                      (... f))]
         [else (if (find?-abstract (first lod) f)
                   (... (dir-name (first lod))
                        (fn-dl-process (dir-dirs (first lod))))
                   (fn-dl-process (rest lod)))]))

     ; Dir -> Boolean
     ; consumes a direectory d and produces true if
     ; it contains the file f
     (define (in-file? d)
       (ormap (lambda (x) (string=? f (file-name x)))
              (dir-files d))))

    (cond
      [(in-file? dir) (... (dir-name dir) f)]
      [else (if (find?-abstract dir f)
                (... (dir-name dir)
                     (fn-dl-process (dir-dirs dir)))
                ...)])))

(define (find-abstract dir f)
  (local
    (; Dir -> Path
     ; consumes a list of Dir lod and produces the path
     (define (dl-process lod)
       (cond
         [(in-file? (first lod)) (cons (dir-name (first lod))
                                       (list f))]
         [else (if (find?-abstract (first lod) f)
                   (cons (dir-name (first lod))
                         (dl-process (dir-dirs (first lod))))
                   (dl-process (rest lod)))]))

     ; Dir -> Boolean
     ; consumes a direectory d and produces true if
     ; it contains the file f
     (define (in-file? d)
       (ormap (lambda (x) (string=? f (file-name x)))
              (dir-files d))))

    (cond
      [(in-file? dir) (cons (dir-name dir) f)]
      [else (if (find?-abstract dir f)
                (cons (dir-name dir)
                      (dl-process (dir-dirs dir)))
                #false)])))

; https://github.com/adaliu-gh/htdp/blob/master/19-24%20Intertwined%20Data/338-344.rkt
; find? and find implementations

(define (find? d f)
  (or (ormap (lambda (x) (string=? f (file-name x))) (dir-files d))
      (ormap (lambda (x) (find? x f)) (dir-dirs d))))

(define (in-files? d f)
  (ormap (lambda (x) (string=? f (file-name x))) (dir-files d)))
    
; Dir String -> [Maybe [Path]]
(define (find d f)
  (local (
          ;; [List-of Dir] -> Dir
          ;; gets the subdirectory where is the f is
          (define (get-sub l)
            (cond
              [(empty? (rest l)) (rest l) ]
              [else (if (find? (first l) f) (first l) (get-sub (rest l)))])))
    (cond
      [(in-files? d f) (list (dir-name d) f)]
      [(find? d f) (cons (dir-name d) (find (get-sub (dir-dirs d)) f))]
      [else #false])))

;; challenge

; Dir -> [Maybe [List-of Path]] or Boolean
; consumes a Dir dir and produces all paths to f or
; false otherwise

(check-expect (find-all test "thisone.txt")
              (list
               (list
                '/home/pc/code/test
                '/home/pc/code/test/second
                "thisone.txt")))

(check-expect (find-all test5 "thisone.txt")
              (list
               (list
                '/home/pc/code/test5
                '/home/pc/code/test5/first
                "thisone.txt")
               (list
                '/home/pc/code/test5
                '/home/pc/code/test5/second
                "thisone.txt")))

(define (fn-find-all dir f)
  (local
    (; Dir -> Dir
     ; consumes a Dir d and produces a Dir for processing
     (define (fn-dl-process d)
       (cond
         [(empty? d) ...]
         [(find?-abstract (first d))
          (... (fn-sub-path (first d))
               (fn-dl-process (rest d)))]
         [else (fn-dl-process (rest d))]))

     ; Dir -> Path
     ; consumes a Dir d and produces the sub-path
     (define (fn-sub-path d)
       (cond
         [(fn-in-file? d) (... (dir-name d) (list f))]
         [else
          (... (dir-name d) (fn-dl-process (dir-dirs d) f))]))

     ; Dir -> Boolean
     ; consumes a Dir d and produces true if it contains f
     (define (fn-in-file? d)
       (ormap (lambda (p) (string=? f (file-name p)))
              (dir-files d))))

    (cond
      [(find?-abstract dir f)
       (map (lambda (q) (... (dir-name dir) q))
            (fn-dl-process (dir-dirs dir)))])))
       
(define (find-all dir f)
  (local
    (; Dir -> Dir
     ; consumes a Dir d and produces a Dir for processing
     (define (dl-process d)
       (cond
         [(empty? d) '()]
         [(find?-abstract (first d) f)
          (cons (sub-path (first d))
                (dl-process (rest d)))]
         [else (dl-process (rest d))]))

     ; Dir -> Path
     ; consumes a Dir d and produces the sub-path
     (define (sub-path d)
       (cond
         [(in-file? d) (cons (dir-name d) (list f))]
         [else
          (cons (dir-name d) (dl-process (dir-dirs d) f))]))

     ; Dir -> Boolean
     ; consumes a Dir d and produces true if it contains f
     (define (in-file? d)
       (ormap (lambda (p) (string=? f (file-name p)))
              (dir-files d))))

    (cond
      [(find?-abstract dir f)
       (map (lambda (q) (cons (dir-name dir) q))
            (dl-process (dir-dirs dir)))]
      [else #false])))

; Exercise 343

; Dir -> List-of Path
; consumes a directory dir and produces the path to
; all files

(check-expect
 (ls-r test3)
 (list
  (list '/home/pc/code/test3 '/home/pc/code/test3/first "happy.txt")
  (list '/home/pc/code/test3 '/home/pc/code/test3/first "test.txt")
  (list '/home/pc/code/test3 '/home/pc/code/test3/first "thisone.txt")
  (list
   '/home/pc/code/test3
   '/home/pc/code/test3/second
   "thisone.txt")))

(check-expect
 (ls-r test4) (list (list '/home/pc/code/test4 "test.txt")))

(define (fn-ls-r dir)
  (local
    (; Dir -> Dir
     ; consumes a Dir d and produces the sub-path
     (define (fn-sub-path d)
       (... 
        (... (dir-name d) (fn-list-files (dir-files d)))
        (... (dir-name d) (fn-dl-process (dir-dirs d)))))

     ; Dir -> Dir
     ; consumes a list of sub-directories dl and produces
     ; a single Dir
     (define (fn-dl-process dl)
       (cond
         [(empty? '()) ...]
         [else
          (... (fn-sub-path (first dl))
               (fn-dl-process (rest dl)))]))
         
     ; File -> List-of Strings
     ; consumes a File f and produces a list of the
     ; names of each file in f
     (define (fn-list-files f)
       (map (lambda (x) (file-name x)) f)))

    (fn-sub-path dir)))

(define (ls-r dir)
  (local
    (; Dir -> Dir
     ; consumes a Dir d and produces the sub-path
     (define (sub-path d)
       (append 
        (cond
          [(empty? d) '()]
          [else
           (append
            (dir-file-path (first d))
            (map (lambda (x) (cons (dir-name (first d)) x))
                 (sub-path (dir-dirs (first d))))
            (sub-path (rest d)))])))

     ; Path -> Path
     ; consumes a Path p and produces a new Path with
     ; the appropriate file at the terminus
     (define (dir-file-path p)
       (map (lambda (q) (foldr (lambda (x y) (cons x y))
                               (list q) (list (dir-name p))))
            (list-files (dir-files p))))
         
     ; File -> List-of Strings
     ; consumes a File f and produces a list of the
     ; names of each file in f
     (define (list-files f)
       (map (lambda (x) (file-name x)) f)))

    (append (map (lambda (y) (cons (dir-name dir) (list (file-name y))))
               (dir-files dir))
          (map (lambda (x) (cons (dir-name dir) x))
               (sub-path (dir-dirs dir))))))



  

























