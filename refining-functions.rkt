;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 20.3 Refining Functions

(require 2htdp/abstraction)
(require htdp/dir)

; Exercise 338

(define test (create-dir "/home/pc/code/test"))
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





 


























