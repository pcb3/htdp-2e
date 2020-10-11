;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fmt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;====
;; 510

(require 2htdp/batch-io)

(define in (read-file "in-file.txt"))
(define out (read-file "out-file.txt"))

; N String String -> String
; consuems a Natural number w, an input file in-f and an output file
; out-f, and produces that output file such that all lines are w in
; width in characters

(check-expect
 (fmt 10 "in-file.txt" "out-file.txt")
 "This is a /nline. Seco/nnd line")

(define (fn-fmt w in-f out-f)
  (local
    (
     (define (width i lo1s seen)
       (cond
         [(empty? lo1s) ...]
         [(= (add1 i)  (length seen))
          (string-append (string-append seen "/n")
                         (width (add1 i) (rest lo1s) ""))]
         [else
          (width (add1 i) (rest lo1s) (... seen (first lo1s)))])))

    (write-file out-f (width w (explode (read-file in-f)) ""))))
  

(define (fmt w in-f out-f)
  (local
    (
     (define (width i lo1s seen)
       (cond
         [(empty? lo1s) seen]
         [(= i w)
          (string-append (string-append seen "/n")
                         (width 0 lo1s ""))]
         [else
          (width (add1 i) (rest lo1s)
                 (string-append seen (first lo1s)))])))

    (read-file
     (write-file out-f (width 0 (explode (read-file in-f)) "")))))
