;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname arrangements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Word -> List-of-words
; creates all rearrangements of the letters in w

(check-expect (arrangements '()) (list '()))

(check-expect (arrangements (list "e" "d"))
              (cons (list "e" "d")
                    (cons  (list "d" "e")
                           '())))

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