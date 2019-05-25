;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname similar-dd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 14.3 Similarities in data definitions

;; ex 239

; A [List X Y] is a structure: 
;   (cons X (cons Y '()))

; A [List Number Number] is a structure:
; (cons Number (cons Number '()))

(define LIST0 (cons 1 (cons 2 '())))

; A [List Number 1String] is a structure:
; (cons Number (cons 1String '()))

(define LIST1 (cons 1 (cons "M" '())))

; A [List Boolean String] is a structure:
; (cons Boolean (cons String '()))

(define LIST2 (cons #true (cons "Happy" '())))

;; ex 240

(define-struct layer [stuff])

; An LStr is one of: 
; – String
; – (make-layer LStr)

(define LSTR1 "hi")
(define LSTR2 (make-layer LSTR1))

; An LNum is one of: 
; – Number
; – (make-layer LNum)

(define LNUM1 42)
(define LNUM2 (make-layer LNUM1))

; A [LAYER ITEM] is a structure:
; - ITEM
; - (make-layer ITEM)

; [LAYER LSTR]
; [LAYER LNUM]

;; ex 241

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures 

; An NEList-of-bools is one of:
; - (cons Bool '())
; - (cons Bool NEList-of-bools)

; A [TB ITEM] is a structure:
; - Boolean
; - CTemperature

; A [List-of [TB ITEM]] is a structure:
; - (cons TB '())
; - (cons TB List-of-[TB ITEM])

; Ex 242. Here is one more parametric data definition:

; A [Maybe X] is one of: 
; – #false 
; – X

; Interpret these data definitions:

; A [Maybe String] is either #false or a string

; A [Maybe [List-of String]] is either #false or a list of strings

; A [List-of [Maybe String]] is a list of Maybe, each structure
; containing either #false or a string

; What does the following function signature mean:

; Accepts a string and a list of strings, outputs a Maybe,
; containing #false or a list of strings

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise

(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))

(check-expect (occurs "a" (list "b" "c" "d")) #f)

(define (fn-occurs s los)
  (...
   [(member? s los)
    (... s los)]
   [... ...]))

(define (occurs s los)
  (cond
    [(member? s los)
     (s-to-end s los)]
    [else #f]))

; String LoS -> LoS
; consumes a string s and a list of strings los,
; and ouptus a list of strings after the first occurence of s in los

(check-expect (s-to-end "a" '("a" "b" "c")) '("b" "c"))

(check-expect (s-to-end "a" '("a")) '())

(define (fn-s-to-end s los)
  (...
   [(empty? (rest los)) (first los)]
   [else (if (string=? s (first los))
             (cons (first (rest los))
                   (fn-s-to-end s (rest los)))
             (fn-s-to-end s (rest los)))]))

(define (s-to-end s los)
  (cond
    [(empty? (rest los)) '()]
    [else (if (string=? s (first los))
              (rest los)
              (s-to-end s (rest los)))]))


























