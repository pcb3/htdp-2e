;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arrangements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

(define WORD1 '())

(define WORD2 (cons "p"
                    (cons "i"
                          (cons "g" '()))))

; A List-of-Words is one of:
; - (list '())
; - (cons Word List-of-Words)
; interpretation a list of words is a list of a list of 1Strings (letters)

(define LOW1 (cons
              (list "a" (cons "b" '()))
              (cons
               (list "c" (cons "d" '()))
               '())))

(define LOW2 (cons
              (list "p" '())
              (cons
               (list "c" '())
               '())))
                   
; Word -> List-of-words
; finds all rearrangements of word

(check-expect (arrangements '()) (list '()))

(check-expect (arrangements (cons "e" (cons "d" '())))
              (cons (list "e" "d")
                    (cons  (list "d" "e")
                           '())))

; Word -> List-of-words
; creates all rearrangements of the letters in w

(define (fn-arrangements w)
  (cond
    [(empty? w) ...]
    [else (... (first w) ...
               ... (fn-arrangements (rest w)) ...)]))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere (first w)
                             (arrangements (rest w)))]))

; 1String List-of-Words -> List-of-Words
; consumes a letter s and a list of words w and outputs
; a word that has s inserted in all posible places
; for each word

(check-expect (insert-everywhere "f" (list '()))
              (cons
               (list "f")
               '()))

(check-expect (insert-everywhere "f" (cons
                                      (list "i" "g")
                                      '()))
              (cons (list "f" "i" "g")
                    (cons (list "i" "f" "g")
                          (cons (list "i" "g" "f")
                                '())))) 

(define (fn-insert-everywhere s w)
  (cond
    [(empty? w) ...]
    [else (... (... s (first w))
               (fn-insert-everywhwere s w))]))

(define (insert-everywhere s w)
  (cond
    [(empty? w) (list '())]
    [else (cons (insert-append s '() (first w))
                (insert-everywhere s (rest w)))]))

; 1String List List-of-Words -> List-of-Words
; consumes a string s an empty list pre, a word post and
; appends s before between and after the word post

(check-expect (insert-append "a" '() (list '()))
              (cons (list "a")
                    '()))

(check-expect (insert-append "a" '() (list "b"))
              (cons (list "a" "b")
                    (cons (list "b" "a")
                          '())))

(check-expect (insert-append "a" (list "b") (list "c"))
              (cons (list "b" "a" "c")
                    (cons (list "b" "c" "a")
                          '())))

;(define (fn-insert-append s pre post)
;  (cond
;    [(empty? post) ...]
;    [else (... (... (... pre s) post)
;               (fn-insert-append s
;                                 (... (... (first post)) pre)
;                                 (rest post)))]))
;
;(define (insert-append s pre post)
;  (cond
;    [(empty? post) (list '())]
;    [else (cons (append (cons s pre) post)
;                (insert-append s
;                               (cons (first (reverse post)) pre)
;                               (rest post)))]))

(define (fn-insert-append s w)
  (cond
    [(empty? w) ...]
    [else (... (... (... s (first w)) (rest w))
               (fn-insert-append s (rest w)))]))

(define (insert-append s w)
  (cond
    [(empty? w)
















