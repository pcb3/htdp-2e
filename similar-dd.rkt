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