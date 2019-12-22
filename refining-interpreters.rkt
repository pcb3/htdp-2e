;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-interpreters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 21 Refining Interpreters

; 21.1 Interpreting Expressions

(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; exercise 345

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)

; (+ 10 -10) -> (make-add 10 -10)
; (+ (* 20 3) 33) -> (make-add (make-mul 20 3) 33)
; (+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))) ->
; (make-add (make-mul 3.14 (make-mul 2 3))
;           (make-mul 3.14 (make-mul -1 -9)))

; (make-add -1 2) -> (+ -1 2)
; (make-add (make-mul -2 -3) 33) -> (+ (* -2 -3) 33)
; (make-mul (make-add 1 (make-mul 2 3)) 3.14) ->
; (* (+ 1 (* 2 3)) 3.14)

; Exercise 346

; an BSL-expr is one of:
; - Number
; - (make-add [BSL-expr BSL-expr])
; - (make-mul [BSL-expr BSL-expr])

(define BSL-EVAL1 (make-add 1 1))
(define BSL-EVAL2 (make-mul 2 3))
(define BSL-EVAL3 (make-add
                   (make-mul BSL-EVAL2 BSL-EVAL1)
                   (make-add BSL-EVAL1 BSL-EVAL1)))

; Exercise 347

; BSL-eval -> Number
; consumes a BSL expression that can evaluate and
; produces that value

(check-expect (eval-expression BSL-EVAL1) (+ 1 1))
(check-expect (eval-expression BSL-EVAL2) (* 2 3))
(check-expect (eval-expression BSL-EVAL3)
              (+ (* (* 2 3) (+ 1 1))
                 (+ (+ 1 1) (+ 1 1))))
(check-expect (eval-expression '$)
              "not a BSL expression")

(define (fn-eval-expression bsl-eval)
  (cond
    [(number? bsl-eval) bsl-eval]
    [(add? bsl-eval)
     (... (fn-eval-expression (add-left bsl-eval))
          (fn-eval-expression (add-right bsl-eval)))]
    [(mul? bsl-eval)
     (... (fn-eval-expression (mul-left bsl-eval))
          (fn-eval-expression (mul-right bsl-eval)))]
    [else ...]))
  
(define (eval-expression bsl-eval)
  (cond
    [(number? bsl-eval) bsl-eval]
    [(add? bsl-eval)
     (+ (eval-expression (add-left bsl-eval))
        (eval-expression (add-right bsl-eval)))]
    [(mul? bsl-eval)
     (* (eval-expression (mul-left bsl-eval))
        (eval-expression (mul-right bsl-eval)))]
    [else "not a BSL expression"]))

; Exercise 348

(define-struct and-expr [left right])
(define-struct or-expr [left right])
(define-struct not-expr [bool])

; A BBSL (short for Boolean BSL) is one of:
; - #true
; - #false
; - and-expr
; - or-expr
; - not-expr

(define BBSL1 #true); -> #true
(define BBSL2 #false); -> #false
(define BBSL3 (make-and-expr #true #false)); -> #false
(define BBSL4 (make-or-expr #true #false)); -> #true
(define BBSL5 (make-not-expr #true)); -> #false

; BBSL -> Boolean
; consumes a representation of BBSL bbsl and
; produces the Boolean value

(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression (make-and-expr #true #true)) (and #true #true))
(check-expect (eval-bool-expression (make-and-expr #true #false)) (and #true #false))
(check-expect (eval-bool-expression (make-or-expr #true #false)) (or #true #false))
(check-expect (eval-bool-expression (make-or-expr #false #false)) (or #false #false))
(check-expect (eval-bool-expression (make-not-expr #true)) (not #true))
(check-expect (eval-bool-expression (make-not-expr #false)) (not #false))
(check-expect (eval-bool-expression (make-and-expr #true (make-not-expr #true))) #false)

(define (eval-bool-expression bbsl)
  (cond
    [(boolean? bbsl) bbsl]
    [(and-expr? bbsl) (and (eval-bool-expression (and-expr-left bbsl))
                           (eval-bool-expression (and-expr-right bbsl)))]
    [(or-expr? bbsl) (or (eval-bool-expression (or-expr-left bbsl))
                         (eval-bool-expression (or-expr-right bbsl)))]
    [(not-expr? bbsl) (not (eval-bool-expression (not-expr-bool bbsl)))]
    [else "not a BBSL expression"]))

; Exercise 349

; An Atom is one of:
; - Number
; - String
; - Symbol

(define (atom? x)
  (cond
    [(number? x) #true]
    [(string? x) #true]
    [(symbol? x) #true]
    [else #false]))

(define WRONG "wrong expression")

(check-expect (parse 0) 0)
(check-expect (parse '(+ 0 0)) (make-add 0 0))
(check-expect (parse '(* 0 0)) (make-mul 0 0))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; S-expr -> Number
; consumes an S-expr and produces its value if it
; is recognised by parse as a BSL-expr expression
; otherwise an error is signaled

(check-expect (interpreter-expr 0) 0)
(check-expect (interpreter-expr '(+ 1 1)) 2)
(check-expect (interpreter-expr '(* 2 3)) 6)

(define (interpreter-expr s)
  (eval-expression (parse s)))

; 21.2 Interpreting Variables

; Exercise 352

; BSL-var-expr Symbol Number -> BSL-var-expr
; consumes a BSL-var-expr ex, a Symbol x, a Number
; v and produces a BSL-var-expr with all
; occurences of x replaced by v

(check-expect (subst 'x 'x 0) 0)
(check-expect (subst (make-add 'x 1) 'x 2)
              (make-add 2 1))
(check-expect (subst (make-mul 'x 2) 'x 3)
              (make-mul 3 2))
(check-expect (subst
               (cons '@ (cons 11 '())) '@ 11)
              (cons 11 (cons 11 '())))
  
(define (subst ex x v)
  (cond
    [(equal? ex x) v]
    [(list? ex)
     (cons (subst (first ex)
                  (first ex) (first (rest ex)))
           (rest ex))]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]
    [else ex]))

; Exercise 353

; BSL-var-expr -> Boolean
; consumes a BSL-var-expr and produces true if
; it is also a BSL-expr

(check-expect (numeric? 0) #true)
(check-expect (numeric? 'p) #false)
(check-expect (numeric? (make-add (make-mul 2 3)
                                  3))
              #true)
(check-expect (numeric? (make-mul 1 'p))
              #false)

(define (fn-numeric? ex)
  (cond
    [(number? ex) ...]
    [(add? ex) (and (fn-numeric? (add-left ex))
                    (fn-numeric? (add-right ex)))]
    [(mul? ex) (and (fn-numeric? (mul-left ex))
                    (fn-numeric? (mul-left ex)))]
    [else ...]))

(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]
    [else #false]))

; Exercise 354

; BSL-var-expr -> Number or error
; consumes a BSL-var-expr ex and produces the
; value if its numeric else signals an error

(check-expect (eval-variable 0) 0)
(check-expect
 (eval-variable (make-add 1 1)) 2)
(check-expect
 (eval-variable (make-mul 2 3)) 6)
(check-error
 (eval-variable (make-mul 'x 1) (error WRONG)))

(define (eval-variable ex)
  (if (numeric? ex)
      (eval-expression ex)
      (error WRONG)))

; An AL (short for association list) is
; [List-of Association].
; An Association is a list of two items:
; (cons Symbol (cons Number '()))

(define AL1 '((a 1) (b 2) (c 3)))

; Exercicse 354

; BSL-var-expr AL -> Number or error
; coonsumes a BSL-var-expr ex and an AL da and
; iteratively applies subst to all Associations

(check-expect (eval-variable* 1 AL1) 1)
(check-expect (eval-variable*
               (make-add 'a 'b) AL1) 3)
(check-expect (eval-variable*
               (make-mul 'b 'c) AL1) 6)

(define (eval-variable* ex da)
  (local
    ((define subst-iter
       (cond
         [(empty? (rest da))
          (subst ex (first (first da))
                 (first (rest (first da))))]
         [else
          (eval-variable*
           (subst ex (first (first da))
                  (first (rest (first da))))
           (rest da))])))
    (eval-variable subst-iter)))

;; abstract using foldl

(define (eval-variable*-abstract ex da)
  (eval-variable
   (foldl (lambda (a b)
            (subst b (first a) (second a)))
          ex da)))

; Exercise 355

; BSL-var-expr AL -> Number
; consumes a BSL-var-expr e and an AL da and
; produces the value of the expression or signals
; an error if there is no substitution

(check-expect (eval-var-lookup 1 AL1) 1)
(check-expect (eval-var-lookup 'a AL1) 1)
(check-expect
 (eval-var-lookup (make-add 'a 'b) AL1) 3)
(check-expect
 (eval-var-lookup (make-mul 'b 'c) AL1) 6)

(define (fn-eval-var-lookup e da)
  (cond
    [(number? e) e]
    [(symbol? e)
     (cond [... (... (boolean? (assq e da))
                     (... ...)
                     (... (assq e da)))])]
    [(add? e)
     (...
      (fn-eval-var-lookup (add-left e) da)
      (fn-eval-var-lookup (add-right e) da))]
    [(mul? e)
     (...
      (fn-eval-var-lookup (add-left e) da)
      (fn-eval-var-lookup (add-right e) da))]
    [else (... ...)]))   

(define (eval-var-lookup e da)
  (cond
    [(number? e) e]
    [(symbol? e)
     (cond [else (if (boolean? (assq e da))
                     (error WRONG)
                     (second (assq e da)))])]
    [(add? e)
     (+
      (eval-var-lookup (add-left e) da)
      (eval-var-lookup (add-right e) da))]
    [(mul? e)
     (*
      (eval-var-lookup (mul-left e) da)
      (eval-var-lookup (mul-right e) da))]
    [else (error WRONG)]))

; 21.3 Interpretig Functions

; Exercise 356

(define-struct fun-expr [name arg])

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
; - (make-fun Symbol BSL-fun-expr)

(define FUN1 (make-fun-expr 'k (make-add 1 1)))
(define FUN2 (make-mul 5 (make-fun-expr 'k (make-add 1 1))))
(define FUN3 (make-mul (make-fun-expr 'i 5)
                       (make-fun-expr 'k (make-add 1 1))))

; Exercise 357

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number or error
; consumes a BSL-fun-expr ex, a symbol f representing
; the funcitons name,
; a symbol x; the functions parameter. And a BSL-fun-expr b
; that represents the function's body

(check-expect (eval-definition2 1 'k 'x 'x) 1)
(check-expect
 (eval-definition2 1 'k 'x (make-add 'x 'x)) 2)
(check-expect
 (eval-definition2 1 'k 'x (make-mul 'x 'x)) 1)
(check-expect
 (eval-definition2 (make-add 1 2)
                   'k 'x (make-add 'x 'x)) 6)
(check-expect
 (eval-definition2 (make-fun-expr 'k (make-add 2 3))
                   'k 'x (make-add 'x 'x)) 10)
(check-expect
 (eval-definition2
  (make-fun-expr 'l (make-fun-expr 'm (make-add 2 3)))
  'k 'x (make-add 'x 'x)) 10)
(check-expect
 (eval-definition2
  (make-fun-expr 'l (make-fun-expr 'm (make-add 2 3)))
  'k 'x (make-fun-expr 'o 1)) 1)
(check-expect
 (eval-definition2
  (make-fun-expr 'l (make-fun-expr 'm (make-add 2 3)))
  'k 'x (make-fun-expr 'o (make-add 'x 'x))) 10)
(check-expect
 (eval-definition2
  (make-fun-expr 'l 'p)
  'k 'x (make-fun-expr 'o 1)) (error WRONG))

(define (fn-eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(add? ex)
     (... (fn-eval-definition1 (add-left ex) f x b)
          (fn-eval-definition1 (add-right ex) f x b))]
    [(mul? ex)
     (... (fn-eval-definition1 (mul-left ex) f x b)
          (fn-eval-definition1 (mul-right ex) f x b))]
    [(fun-expr? ex)
     (... (... (fun-expr-name ex) f)
          (fn-eval-definition1
           (subst b x
                  (fn-eval-definition1 (fun-expr-arg ex)))
           f x b)
          ...)]
    [else ...]))

(define (eval-definition1 ex f x b)
  (local
    ((define (eval-arg ag)
       (cond
         [(number? ag) ag]
         [(add? ag)
          (+ (eval-definition1 (add-left ag) f x b)
             (eval-definition1 (add-right ag) f x b))]
         [(mul? ag)
          (* (eval-definition1 (mul-left ag) f x b)
             (eval-definition1 (mul-right ag) f x b))]
         [(fun-expr? ag)
          (eval-definition1
           (subst b x
                  (eval-definition1
                   (fun-expr-arg ag) f x b))
           f x b)]
         [else (error WRONG)])))
    (eval-expression (subst b x (eval-arg ex)))))

(define (eval-definition2 ex f x b)
  (local
    ((define (eval-arg ag)
       (cond
         [(number? ag) ag]
         [(add? ag)
          (+ (eval-arg (add-left ag))
             (eval-arg (add-right ag)))]
         [(mul? ag)
          (* (eval-arg (mul-left ag))
             (eval-arg (mul-right ag)))]
         [(fun-expr? ag)
          (eval-arg (fun-expr-arg ag))]
         [else (error WRONG)]))
     ;(define arg (eval-arg ex))
     (define value (eval-arg ex))
     (define plugd (subst (if (fun-expr? b)
                              (fun-expr-arg b)
                              b) x value)))
    (eval-expression plugd)))

; Exercise 358

(define-struct fun-def [name param expr])

; A BSL-fun-def is a structure:
; - (make-fun-def Symbol Symbol BSL-fun-expr
; Interpretation: A BSL-fun-def is a representation
; for function definitions

(define FUN-DEF1 (make-fun-def 'f 'x (make-add 3 'x)))
(define FUN-DEF2
  (make-fun-def 'g 'y
                (make-fun-expr 'f (make-mul 2 'y))))
(define FUN-DEF3
  (make-fun-def 'h 'v
                (make-add (make-fun-expr 'f 'v)
                          (make-fun-expr 'g 'v))))

; a BSL-fun-def* is one of:
; - '()
; - (cons BSL-fun-def (cons BSL-fun-def* '()))

(define da-fgh (list FUN-DEF1 FUN-DEF2 FUN-DEF3))












































