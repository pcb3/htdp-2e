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



























