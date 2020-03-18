;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Intermezzo 4: The Nature of Numbers


;;=============================
;; Fixed-Size Number Arithmetic

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

;;====
;; 412

; Inex Inex -> Inex
; consumes two Inex and adds them together
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
 (create-inex 3 1 0))

; integer division by 10 and adding 1 to exponent if
; mantissa > 99
(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))

; rounding to nearest inex
(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))

(check-error
 (inex+ MAX-POSITIVE MAX-POSITIVE)
 "result out of range")
 
(define (fn-inex+ inex1 inex2)
  (local
    (
     ; Inex Inex -> Number
     ; raw number by adding both mantissi
     (define raw-add
       (+ (inex-mantissa ...) (inex-mantissa ...)))
     
     ; Inex Inex -> Inex
     ; create a new inex from two inex representations
     (define build-inex
       (cond
         [else
          (if (> raw-add ...)
              (create-inex
               (round (/ raw-add ...))
               (inex-sign ...) (add1 (inex-exponent ...)))
              (create-inex raw-add
                           (inex-sign ...)
                           (inex-exponent ...)))])))

    build-inex))
              
(define (inex+ inex1 inex2)
  (local
    (
     ; checks that Inex is within MAX/MIN-POSITIVE range
     (define exceeds-range?
       (or
        (> (+ (inex->number inex1) (inex->number inex2))
           (inex->number MAX-POSITIVE))
        (< (+ (inex->number inex1) (inex->number inex2))
           (inex->number MIN-POSITIVE))))
          
        
     ; raw number by adding both mantissi
     (define raw-add
       (+ (inex-mantissa inex1) (inex-mantissa inex2)))
     
     ; create a new inex from two inex representations
     (define build-inex
       (cond
         [exceeds-range? (error "result out of range")]
         [else
          (if (> raw-add (inex-mantissa MAX-POSITIVE))
              (create-inex
               (round (/ raw-add 10))
               (inex-sign inex1) (add1 (inex-exponent inex1)))
              (create-inex raw-add
                           (inex-sign inex1)
                           (inex-exponent inex1)))])))

    build-inex))


















