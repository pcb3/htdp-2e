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
;; 396

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; HM-Word N -> String
; runs a simplistic hangman game, produces the current
; state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list
                             (length the-word) "_"))
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

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
;(play (list-ref AS-LIST (random SIZE)) 10)

;;========================
;; 397

(define-struct employee [name ssn payrate])
; an employee is a structure:
; (make-employee (String Number Number)
; Interpretation: employee is a record including their
; pay rate in $USD per hour

(define EMPLOYEE0 (make-employee "freddy" 123 75))
(define EMPLOYEE1 (make-employee "jason" 888 4))

(define-struct time-card [ssn hours])
; a time-card is a structure:
; (make-time-card (Number Number))
; Interpretation: a time-card is a an electronic record
; of an employee's number and hours worked that week

(define TC0 (make-time-card 123 30))
(define TC1 (make-time-card 888 120))

(define-struct wr [name wage])
; a Wr is a structure:
; (make-wr (String Number))
; Interpretation: a wr is a record of the name and wage
; of an employee

(define WR0 (make-wr "freddy" (* 75 30)))
(define WR1 (make-wr "jason" (* 4 120)))

(define NOT-FOUND "entry not found")

; [List-of Employee] [List-of Time-card] -> [List-of Wr]
; consumes a list of Employee records ler and a list
; of Time-card ltc, and produces a list of Wr
(check-expect (wages '() '()) '())
(check-expect
 (wages `(,EMPLOYEE0 ,EMPLOYEE1) `(,TC0 , TC1))
 `(,WR0 , WR1))
;(check-expect
; (wages `(,(make-employee "pc" 999 999)) `(,TC0))
; (error NOT-FOUND))
;(check-expect
; (wages `(,EMPLOYEE0)
;        `(,TC0 ,(make-time-card 666 10000)))
; (error NOT-FOUND))

(define (fn-wages ler ltc)
  (cond
    [(and (empty? ler) (empty? ltc)) ...]
    [(and (cons? ler) (cons? ltc))
     (cond
       [else
        (if
         (= (employee-ssn (first ler))
            (time-card-ssn (first ltc)))
         (...
          (make-wr (employee-name (first ler))
                   (* (employee-payrate (first ler))
                      (time-card-hours (first ltc))))
          (fn-wages (rest ler) (rest ltc)))
         (fn-wages (first ler) (rest ltc)))])]
    [else ...]))
                
(define (wages ler ltc)
  (cond
    [(and (empty? ler) (empty? ltc)) '()]
    [(and (cons? ler) (cons? ltc))
     (cond
       [else
        (if
         (= (employee-ssn (first ler))
            (time-card-ssn (first ltc)))
         (cons
          (make-wr (employee-name (first ler))
                   (* (employee-payrate (first ler))
                      (time-card-hours (first ltc))))
          (wages (rest ler) (rest ltc)))
         (wages (first ler) (rest ltc)))])]
    [else (error NOT-FOUND)]))

;;================================
;; 398

; [List-of Number] [List of Number] -> Number
; consumes a list of coefficients loc and a list of
; variable values lov and produces the value of the
; linear combination
(check-expect (value '(0) '(0)) 0)
(check-expect (value '(2 3 5) '(1 2 3)) 23)

(define (fn-value loc lov)
  (cond
    [(empty? loc) ...]
    [else (... (... (first loc) (first lov))
               (fn-value (rest loc) (rest lov)))]))

(define (value loc lov)
  (cond
    [(empty? loc) 0]
    [else (+ (* (first loc) (first lov))
             (value (rest loc) (rest lov)))]))

;;================================
;; 399

(check-random (random-pick '(1 2 3))
              (list-ref '(1 2 3) (random 3)))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (local
    ((define (select num rand-num li)
       (cond
         [(= num rand-num) (first li)]
         [else (select (add1 num) rand-num (rest li))])))
    (select 0 (random (length l)) l)))
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place
(check-expect (non-same '(1 2 3)
                        '((1 2 3)
                          (1 3 2)
                          (3 2 1) (3 1 2) (2 1 3)))
              '((3 1 2)))

(define (non-same names ll)
  (cond
    [(empty? ll) '()]
    [(boolean=? #true
                (same-in-a-position (first ll) names))
     (non-same names (rest ll))]
    [else
     (cons (first ll) (non-same names (rest ll)))]))

(define (same-in-a-position l lnames)
  (cond
    [(empty? lnames) #false]
    [else (if (equal? (first l) (first lnames))
              #true
              (same-in-a-position (rest l)
                                  (rest lnames)))]))
     
;;============================
;; 400

; [List-of Symbol] [List-of Symbol] -> Boolean
; consumes two lists of DNA symbols, a pattern p and
; search string s and produces true if p is
; identical to intial part of s
(check-expect (DNAprefix '(a g t c) '(a g t c a g))
              #true)
(check-expect (DNAprefix '(a c g t) '(c c g t a g))
              #false)

(define (fn-DNAprefix p s)
  (cond
    [(empty? p) ...]
    [else (... (symbol=? (first p) (first s))
               (fn-DNAprefix (rest p) (rest s))
               ...)]))

(define (DNAprefix p s)
  (cond
    [(empty? p) #true]
    [else (if (symbol=? (first p) (first s))
              (DNAprefix (rest p) (rest s))
              #false)]))    

; [List-of Symbol] [List-of Symbol] -> Symbol
; consumes a pattern p and search string s and produces
; the first item in s beyond p
(check-expect (DNAdelta '(a g t c) '(a g t c a t c))
              'a)
;(check-expect (DNAdelta '(a g t c) '(a g t c))
;              (error "identical sequence"))
(check-expect (DNAdelta '(a g t c) '(t g t c a t c))
              #false)

(define (fn-DNAdelta p s)
  (cond
    [(equal? p s) ...]
    [(empty? p) (first s)]
    [else (if (symbol=? (first p) (first s))
              (fn-DNAdelta (rest p) (rest s))
              ...)]))
     
(define (DNAdelta p s)
  (cond
    [(equal? p s) (error "identical sequence")]
    [(empty? p) (first s)]
    [else (if (symbol=? (first p) (first s))
              (fn-DNAdelta (rest p) (rest s))
              #false)]))

;;==========================================
;; 401

; S-expr S-expr -> Boolean
; consumes two S-expressions and produces true if they
; are equal
(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? 1 0) #false)
(check-expect (sexp=? '(a) '(a)) #true)
(check-expect (sexp=? 'a '(a)) #false)
(check-expect (sexp=? '(1 a "p") '(1 a "p")) #true)
(check-expect (sexp=? '(1 a "z") '(1 a "m")) #false)

(define (atom? a)
  (or (number? a) (string? a) (symbol? a)))

(define (fn-sexp=? a b)
  (cond
    [(and (empty? a) (empty? b)) ...]
    [(or (and (atom? a) (atom? b))
         (and (cons? a) (cons? b))) ...]
    [(or (and (atom? a) (cons? b))
         (and (cons? a) (atom? b))) ...]))

(define (sexp=? a b)
  (cond
    [(and (empty? a) (empty? b)) #true]
    [(and (atom? a) (atom? b)) (equal? a b)]
    [(and (cons? a) (cons? b))
     (and (sexp=? (first a) (first b))
          (sexp=? (rest a) (rest b)))]
    [else #false]))

;;=================================
;; 402

; If we treat the expression as an atomic value
; we can simplify the function into a list
; processor rather than a function with two
; complex inputs. Also, the expression when evaluated
; becomes a number so we can temporarily treat it as
; such


























