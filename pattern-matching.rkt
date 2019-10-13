;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pattern-matching) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Pattern matching

(require 2htdp/abstraction)

; [List-of Posn] -> [List-of Posn] 
; moves each object right by delta-x pixels

(define (move-right lop delta-x)
  (for/list [(p lop)]
       (make-posn
        (+ (posn-x p) delta-x) (posn-y p))))

; Exercise 308

> (define-struct phone [area switch four])
> (match (make-phone 713 664 9993)
    [(phone x y z) (+ x y z)])

11370

> (match (cons (make-phone 713 664 9993) '())
    [(cons (phone area-code 664 9993) tail)
     area-code])

713

; List-of Phone Number -> List-of Phone
; consumes a list of phone numbers loph and
; substitutes the area code

(define input-phone
  `(,(make-phone 101 888 8888)
    ,(make-phone 102 111 1111)))
(define expect-phone
  `(,(make-phone 999 888 8888)
    ,(make-phone 999 111 1111)))                                               

(check-expect (replace 999 input-phone) expect-phone)

(define (replace area loph)
  (for/list ((p loph))
    (match p
      [(phone x y z) (make-phone area y z)])))








