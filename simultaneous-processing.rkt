;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simultaneous-processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;===========================
;; 23 Simultaneous Processing

;;================================================
;; 23.1 Processing Two Lists Simultaneously: Case1

;;==============
;; 387

; [List-of Symbol] [List-of Number] -> [List-of List]
; consumes a list of Symbols los and a list of Numbers
; lon and produces all possible oredered pairs
(check-expect (cross '(a) '(1 2 3)) '((a 1) (a 2) (a 3)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b) '(1 2))
              '((a 1) (a 2) (b 1) (b 2)))

(define (fn-cross los lon)
  (cond
    [(empty? los) ...]
    [else
     (... (... (first los) lon)
          (fn-cross (rest los) lon))]))
    
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else
     (append
      (map (lambda (p) (cons (first los) (list p))) lon)
      (cross (rest los) lon))]))