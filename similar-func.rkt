;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname similar-func) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 14.1 similarities in funcitons

; String LoS -> Boolean
; determines wether l contains s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else
     (or (string=? (first l) s)
         (contains? s (rest l)))]))
           