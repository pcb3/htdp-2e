;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname complex-inputs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================
;; 23.5 Designing functions that
;;      consume two complex inputs

(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

;;==================================
;; 23.6 Finger exercises: two inputs

;;====
;; 393

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
 
; Constraint If s is a Son.R, 
; no number occurs twice in s

(define SON.R0 '())
(define SON.R1 '(1 3 5))

; SON.R SON.R -> SON.R
; consumes two sets s1 s2 and produces the union
(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(1)) '(1))
(check-expect (sort (union '(1 3 5) '(2 4 6)) <)
              '(1 2 3 4 5 6))

(define (fn-union s1 s2)
  (cond
    [(empty? s1) ...]
    [else
     (if (member? (first s1) ...)
         (fn-union (rest s1) ...)
         (cons (first s1) (fn-union (rest s1) ...)))]))

(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [else
     (if (member? (first s1) s2)
         (union (rest s1) s2)
         (cons (first s1) (union (rest s1) s2)))]))

; SON.R SON.R -> SON.R
; consumes two sets s1 s2 and produces their intersection
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '() '(1)) '())
(check-expect (intersect '(1) '(2)) '())
(check-expect (intersect '(1 2) '(2 3)) '(2))

(define (fn-intersect s1 s2)
  (cond
    [(empty? s1) ...]
    [else (if (member? (first s1) ...)
              (cons (first s1)
                    (fn-intersect (rest s1) ...))
              (fn-intersect (rest s1) ...))]))

(define (intersect s1 s2)
  (cond
    [(empty? s1) '()]
    [else (if (member? (first s1) s2)
              (cons (first s1)
                    (intersect (rest s1) s2))
              (intersect (rest s1) s2))]))

;;=======================
;; 394

; SON.L SON.L -> SON.L
; consumes two sets s1 s2 and produces the union of
; both including repeats
(check-expect (merge '() '()) '())
(check-expect (merge '() '(1)) '(1))
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '(1 2) '(3 4)) '(1 2 3 4))
(check-expect (merge '(1 1 2) '(3 3 4)) '(1 1 2 3 3 4))             

(define (fn-merge s1 s2)
  (cond
    [(and (cons? ...) (cons? ...))
     (...
      (first s1)
      (... (first s2) (fn-merge (rest s1) (rest s2))))]
    [(and (cons? ...) (empty? ...))
     (... (first s1) (fn-merge (rest s1) ...))]
    [(and (empty? ...) (cons? ...))
     (... (first s2) (fn-merge ... (rest s2)))]
    [(and (empty? ...) (empty? ...))
     ...]))

(define (merge s1 s2)
  (cond
    [(and (cons? s1) (cons? s2))
     (cond
       [else (if (<= (first s1) (first s2))
                 (cons
                  (first s1)
                  (merge (rest s1) s2))
                 (cons (first s2) (merge s1) (rest s2)))])]
    [(and (cons? s1) (empty? s2))
     (cons (first s1) (merge (rest s1) s2))]
    [(and (empty? s1) (cons? s2))
     (cons (first s2) (merge s1 (rest s2)))]
    [(and (empty? s1) (empty? s2)) '()]))

;;=======================
;; 395

; List N -> List
; consumes a list l and a natural number n and produces
; the first n items from l or all l if it is too short
(check-expect (take '(a b c) 0) '())
(check-expect (take '(a b c) 2) '(a b))
(check-expect (take '(a b c) 10) '(a b c))

(define (fn-take l n)
  (cond
    [(and (empty? l) (= n 0)) ...]
    [(and (empty? l) (> n 0)) ...]
    [(and (cons? l) (= n 0)) ...]
    [(and (cons? l) (> n 0))
     (... (first l) (fn-take (rest l) (sub1 n)))]))

(define (take l n)
  (if (and (cons? l) (> n 0))
      (cons (first l) (take (rest l) (sub1 n)))
      '()))

;;=====
;; drop

; List N -> List
; consumes a list l and a natural number n and produces
; l with first n items removed or the empty list if the
; length of l is less than or equal to n
(check-expect (drop '() 1) '())
(check-expect (drop '(a b c) 0) '(a b c))
(check-expect (drop '(a b c) 2) '(c))
(check-expect (drop '(a b c) 3) '())
(check-expect (drop '(a b c) 10) '())

(define (fn-drop l n)
  (cond
    [(and (empty? l) (= n 0)) ...]
    [(and (empty? l) (> n 0)) ...]
    [(and (cons? l) (= n 0)) ...]
    [(and (cons? l) (> n 0))
     (fn-drop (rest l) (sub1 n))]))

(define (drop l n)
  (cond
    [(and (or (> n 0) (= n 0)) (empty? l)) l]
    [(and (cons? l) (= n 0)) l]
    [(and (cons? l) (> n 0))
     (drop (rest l) (sub1 n))]))
  
;;==============================

;;====
;; 396

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word 1String -> HM-word
; consumes the target word w, a word s that represents
; the partially guessed word and the current letter
; guessed l and produces s with all "_" representing
; undiscovered letters
(check-expect
 (compare-word '("h" "i") '("_" "_") "o") '("_" "_"))
(check-expect
 (compare-word '("h" "i") '("_" "_") "h") '("h" "_"))
(check-expect
 (compare-word '("h" "i") '("h" "_") "i") '("h" "i"))

(define (fn-compare-word w s l)
  (cond
    [(empty? w) ...]
    [(string=? (first w) ...) ...]
    [else
     (fn-compare-word (rest w) (rest s) ...)]))
    
(define (compare-word w s l)
  (cond
    [(empty? w) '()]
    [(string=? (first w) l)
     (cons l (compare-word (rest w) (rest s) l))]
    [(string=? "_" (first s))
     (cons "_" (compare-word (rest w) (rest s) l))]
    [else
     (cons (first s)
           (compare-word (rest w) (rest s) l))]))










































