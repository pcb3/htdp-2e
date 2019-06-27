;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname heart-of-the-problem) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; exercise 259
; Word games, the heart of the problem

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; Word -> List-of-words
; creates all arrangements of the letters in w
(define (fn-arrangements w)
  (cond
    [(empty? w) ...]
    [else (... (first w) ...
               ... (fn-arragnements (rest w)) ...)]))

(define

;(define (arrangements w)
;  (local (; 1String List-of-words -> List-of-words
;          ; inserts the first letter of w into every possible place
;          (define (insert-everywhere/in-all-words 1str word-list)
;            (cond
;              [(empty? word-list) '()]
;              [else
;               (append (insert-1str '() (first (first word-list)) (first word-list))
;                       (insert-everywhere/in-all-words (rest word-list)))]))
;
;          ; word 1String Word -> Word
;          ; appends prefix and 1str to word  
;          (define (insert-1str prefix 1str word)
;            (cond
;              [(empty? w) (append prefix w)]
;              [else (cons prefix (cons 1str (rest word)))