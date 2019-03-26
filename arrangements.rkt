;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arrangements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Word -> List-of-words
; creates all rearrangements of the letters in w

;(check-expect (arrangements '()) (list '()))
;
;(check-expect (arrangements (cons (list "e" "d") '()))
;              (cons (list "e" "d")
;                    (cons  (list "d" "e")
;                           '())))
;
;(define (fn-arrangements w)
;  (cond
;    [(empty? w) ...]
;    [else (... (first w) ...
;               ... (fn-arrangements (rest w)) ...)]))

(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere (first w)
                             (arrangements (rest w)))]))

; 1String List-of-Words -> List-of-Words
; consumes a letter s and a list of words w and outputs
; a word that has s inserted in all posible places
; for each word

;(check-expect (insert-everywhere "f" (list '()))
;              (cons
;               (list "f")
;               '()))
;
;(check-expect (insert-everywhere "f" (cons
;                                      (list "i" "g")
;                                      '()))
;              (cons (list "f" "i" "g")
;                    (cons (list "i" "f" "g")
;                          (cons (list "i" "g" "f")
;                                '()))))
;
;(define (fn-insert-everywhere s w)
;  (cond
;    [(empty? w) ...]
;    [else (... (... s '() (first w))
;               (fn-insert-everywhwere s w))]))

(define (insert-everywhere s w)
  (cond
    [(empty? w) '()]
    [else (cons (insert-append s '() (first w))
                (insert-everywhere s (rest w)))]))

; 1String Word Word -> List-of-Words
; consumes a 1String s, a Word pre, a word w with the first
; letter removed, and inserts s before between and after each
; letter of w

;(check-expect (insert-append "w" '() '())
;              (cons (list "w") '()))
;
;(check-expect (insert-append "q" '() (list "r"))
;              (cons (list "q" "r")
;                    (cons (list "r" "q")
;                          '())))
;
;(check-expect (insert-append "x" '() (list "y" "z"))
;              (cons (list "x" "y" "z")
;                    (cons (list "y" "x" "z")
;                          (cons (list "y" "z" "x")
;                                '()))))
;
;(define (fn-insert-append s pre word)
;  (cond
;    [(empty? word) ...]
;    [else (... (... s pre word)
;               (fn-insert-append s (... pre (... (first word) '()))
;                                 (rest word)))]))

(define (insert-append s pre word)
  (cond
    [(empty? word) (cons (create-permutation s pre '()) '())]
    [else (cons (create-permutation s pre word)
                (insert-append s (append pre (cons (first word) '()))
                               (rest word)))]))


; 1String Word Word -> Word
; consumes a letter char, a word pre, and a word post, and inserts
; char between pre and post

;(check-expect (create-permutation "o" '() (list "m"))
;              (list "o" "m"))
;
;(check-expect (create-permutation "o" (list "s" "m") '())
;              (list "s" "m" "o"))
;
;(check-expect (create-permutation "o" (list "s") (list "m"))
;              (list "s" "o" "m"))
;
;(define (fn-create-permutation char pre post)
;  (... pre (... char '()) post))

(define (create-permutation char pre post)
  (append pre (cons char '()) post))

(arrangements (list "p" "c" "b"))









