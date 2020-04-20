;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ls1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

; Exercise 338

(define test (create-dir "/home/pc/code/test"))

(define (ls1 dir)
  (local
    ((define (map-dir-name d)
       (map (lambda (p) (dir-name p)) d))

     (define (map-files f)
       (map (lambda (q) (file-name q)) f))

     (define (map-dirs di)
       (map (lambda (r) (ls1 r)) di)))

    (cons (dir-name dir)
          (append (map-dir-name (dir-dirs dir))
                  (map-files (dir-files dir))
                  (map-dirs (dir-dirs dir))))))

(ls1 test)