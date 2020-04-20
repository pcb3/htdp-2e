;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

;; Refining-Interpreters again

(define WRONG "wrong expression")
(define NOT-FOUND "this variable is not defined")

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun-expr [name arg])
(define-struct fun-def [name param expr])

; A BSL-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
; - (make-fun-expr Symbol BSL-expr)

(define CLOSE-TO-PI (make-fun-expr 'close-to-pi 3.145))

;;;;;;;;;;;;;;;;;;
;; eval-expression

; BSL-expr -> Number
; consumes an expression ex and produces its value
(check-expect
 (eval-expression (make-add (make-mul 1 2) 2)) 4)

(define (fn-eval-expression ex)
  (cond
    [(number? ex) ...]
    [(add? ex)
     (... (fn-eval-expression (add-left ex))
          (fn-eval-expression (add-right ex)))]
    [(mul? ex)
     (... (fn-eval-expression (mul-left ex))
          (fn-eval-expression (mul-right ex)))]
    [else (error ...)]))

(define (eval-expression ex)
  (cond
    [(number? ex) ex]
    [(add? ex)
     (+ (eval-expression (add-left ex))
        (eval-expression (add-right ex)))]
    [(mul? ex)
     (* (eval-expression (mul-left ex))
        (eval-expression (mul-right ex)))]
    [(fun-expr? ex)
     (eval-expression (fun-expr-arg ex))]
    [else (error "not a BSL expression")]))

;;;;;;;;
;; parse

(define (atom? a)
  (if (or (number? a) (string? a) (symbol? a))
      #true
      #false))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))
 
; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(and (< L 3) (symbol? (first s)))
       (make-fun-expr (parse (first s))
                      (parse (second s)))]
      [(= L 3)
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [(and (symbol? (first s)) (symbol? (second s)))
          (make-fun-def (parse (first s))
                        (parse (second s))
                        (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))
 
; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) s]))

;;;;;;;;;;;;;;;;;;;
;; interpreter-expr

; S-expr -> Number or error
; consumes an S-expr sexpr and produces its value if it
; is a BSL-expr
(check-expect (interpreter-expr 0) 0)
(check-expect (interpreter-expr '(+ 1 1)) 2)
(check-expect (interpreter-expr '(* 2 2)) 4)
;(check-expect (interpreter-expr 's) (error WRONG))
;(check-expect (interpreter-expr "hi") (error WRONG))

(define (interpreter-expr sexpr)
  (eval-expression (parse sexpr)))

;;;;;;;;
;; subst

; BSL-expr Symbol Number -> BSL-expr
; consumes an expression ex a symbol x and a number
; v and produces an expression with all occurrencese
; of x replaced by v
(check-expect (subst (make-add 'x 'x) 'x 1)
              (make-add 1 1))

(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex)
     (cond (else (if (symbol=? ex x) v x)))]
    [(add? ex)
     (make-add (subst (add-left ex) x v)
               (subst (add-right ex) x v))]
    [(mul? ex)
     (make-mul (subst (mul-left ex) x v)
               (subst (mul-right ex) x v))]
    [else ex]))

;;;;;;;;;;;
;; numeric?

; BSL-expr -> Boolean
; consumes an expression and produces true if there are no
; variables
(check-expect (numeric? 1) #true)
(check-expect (numeric? (make-mul 2 2)) #true)
(check-expect (numeric? (make-add 'a 1)) #false)

(define (fn-numeric? ex)
  (cond
    [(number? ex) ...]
    [(add? ex) (... (fn-numeric? (add-left ex))
                    (fn-numeric? (add-right ex)))]
    [(mul? ex) (... (fn-numeric? (mul-left ex))
                    (fn-numeric? (mul-right ex)))]
    [else ...]))

(define (numeric? ex)
  (cond
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]
    [else #false]))


; An AL (association list) is [List-of Association]
; An Association is a list of two items:
;   (cons Symbol (cons Number '()))

;;;;;;;;;;;;;;;;
;; eval-variable

; BSL-expr AL -> NUmber or error
; consumes an expression ex and produces its value if its
; numeric else signals an error
(check-expect (eval-variable (make-add 1 'b)) "error")
(check-expect (eval-variable (make-add 1 10)) 11)

(define (eval-variable ex)
  (if (numeric? ex) (eval-expression ex) "error"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-variable* and eval-var lookup combo

; BSL-expr AL -> Number
; consumes an expression ex and an association list da
; and produces the value of ex if its numeric by
; substution of all variables in ex for values in da
(check-expect (eval-variable* 'a (list '(a 1))) 1)
(check-expect (eval-variable*
               (make-add 'a 'b) (list '(a 1) '(b 2))) 3)
(check-expect (eval-variable* 'p (list '(a 1) '(b 2)))
              "error")

(define (eval-variable* ex da)
  (local
    (; Symbol -> Number
     ; consumes a Symbol x and an AL al and substitutes
     ; x for a value if x is in da
     
     (define (fn-iter-subst x al)
       (cond
         [(empty? al) ...]
         [else (if (symbol=? x (first (first al)))
                   (first (reverse (first al)))
                   (fn-iter-subst x (rest al)))]))

     (define (iter-subst x al)
       (cond
         [(empty? al) x]
         [else (if (symbol=? x (first (first al)))
                   (first (reverse (first al)))
                   (iter-subst x (rest al)))]))

     ; BSL-expr -> BSL-expr
     ; consumes an expression expr and produces a new
     ; expression with all possible substitutions of
     ; variables for values
     
     (define (fn-sub-all expr)
       (cond
         [(symbol? expr) (iter-subst expr da)]
         [(add? expr)
          (make-add (fn-sub-all (add-left ...))
                    (fn-sub-all (add-right ...)))]
         [(mul? expr)
          (make-mul (fn-sub-all (mul-left ...))
                    (fn-sub-all (mul-right ...)))]
         [else ...]))

     (define (sub-all expr)
       (cond
         [(symbol? expr) (iter-subst expr da)]
         [(add? expr)
          (make-add (sub-all (add-left expr))
                    (sub-all (add-right expr)))]
         [(mul? expr)
          (make-mul (sub-all (mul-left expr))
                    (sub-all (mul-right expr)))]
         [else expr])))

    (eval-variable (sub-all ex))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreting functions

;;;;;;;;;;;;;;;;;;;
;; eval-definition1

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; consumes a BSL-fun-expr ex, the name of the applied
; function and its argument.
; A Symbol f; the functions name, parameter x, and body b
; and produces the value of an application of f to some
; argument
(check-expect
 (eval-definition1 (make-fun-expr 'f 1) 'f 'x 'x) 1)
(check-expect
 (eval-definition1
  (make-fun-expr
   'f (make-add 1 1)) 'f 'x (make-mul 'x 'x)) 4)
(check-expect
 (eval-definition1
  CLOSE-TO-PI 'f 'x (make-add 'x (make-mul 'x 'x)))
 (+ (fun-expr-arg CLOSE-TO-PI)
    (* (fun-expr-arg CLOSE-TO-PI)
       (fun-expr-arg CLOSE-TO-PI))))

;  (local ((define value (eval-definition1 ex f x b))
;          (define plugd (subst b x value)))
;       (eval-definition1 plugd f x b)))

(define (eval-definition1 fexpr f x b)
  (eval-expression (subst b x (eval-expression fexpr))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A BSL-fun-def is a structure:
; - (make-fun-def Symbol Symbol BSL-fun-expr)
; Interpretation: A BSL-fun-def is a representation
; for function definitions

(define
  AREA-OF-CIRCLE
  (make-fun-def
   'area-of-circle 'r (make-mul
                       CLOSE-TO-PI
                       (make-mul 'r 'r))))

(define VOLUME-OF-10-CYLINDER
  (make-fun-def
   'volume-of-10-cylinder 'r (make-mul
                              10
                              AREA-OF-CIRCLE)))

; A BSL-fun-def* is one of:
; - '()
; - (cons BSL-fun-def BSL-fun-def*)

(define BSL-FUN-DEF*1 (list AREA-OF-CIRCLE
                            VOLUME-OF-10-CYLINDER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def BSL-FUN-DEF*1 'area-of-circle)
              AREA-OF-CIRCLE)

(define (lookup-def da f)
  (first (filter
          (lambda (x) (symbol=? (fun-def-name x) f)) da)))

;;;;;;;;;;;;;;;;;
;; eval-function*

; BSL-fun-expr BSL-fun-def* -> BSL-fun-def or Number
; consumes a fun-expr fex (make-fun-expr name arg)
; and a definitions area da and produces the value of fex
(check-expect
 (eval-function*
  (make-fun-expr 'area-of-circle 1) BSL-FUN-DEF*1)
 (* (fun-expr-arg CLOSE-TO-PI) (* 1 1)))
               
(define (eval-function* fex da)
  (eval-expression
   (subst
    (fun-def-expr
     (lookup-def da (fun-expr-name fex)))
    (fun-def-param
     (lookup-def da (fun-expr-name fex)))
    (fun-expr-arg fex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreting everything

; a BSL-da-all is one of:
; - '()
; - (cons BSL-fun-expr (cons BSL-da-all '()))
; - (cons BSL-fun-def (cons BSL-da-all '()))

(define BSL-DA-ALL1
  (list CLOSE-TO-PI AREA-OF-CIRCLE VOLUME-OF-10-CYLINDER))

;;;;;;;;;;;;;;;;;
;; lookup-con-def

; BSL-da-all Symbol -> BSL-expr
; consumes a definitions area and a constant defintion name
; and produces the value or an error if not found
(check-expect (lookup-con-def BSL-DA-ALL1 'close-to-pi)
              CLOSE-TO-PI)

(define (lookup-con-def da x)
  (cond
    [(empty? da) (error NOT-FOUND)]
    [else (if (and (fun-expr? (first da))
                   (symbol=? x (fun-expr-name (first da))))
              (first da)
              (lookup-con-def (rest da) x))]))

;;;;;;;;;;;;;;;;;
;; lookup-fun-def

; BSL-da-all Symbol -> BSL-expr
; consumes a definitions area and a function defintion name
; and produces the definition or an error if not found
(check-expect (lookup-fun-def BSL-DA-ALL1 'area-of-circle)
              AREA-OF-CIRCLE)

(define (lookup-fun-def da x)
  (cond
    [(empty? da) (error NOT-FOUND)]
    [else (if (and (fun-def? (first da))
                   (symbol=? x (fun-def-name (first da))))
              (first da)
              (lookup-fun-def (rest da) x))]))

;;;;;;;;;;;
;; eval-all

; BSL-fun-expr BSL-da-all -> Number
; consumes an expression ex and a BSL-da-all da and
; produces the value of ex if its contained within da
(check-expect (eval-all (make-fun-expr 'close-to-pi 3.145)
                        BSL-DA-ALL1)
              (fun-expr-arg CLOSE-TO-PI))

(check-expect (eval-all (make-fun-expr 'area-of-circle 1)
                        BSL-DA-ALL1)
              (* (fun-expr-arg CLOSE-TO-PI) (* 1 1)))

(check-expect (eval-all (make-fun-expr
                         'volume-of-10-cylinder 1)
                        BSL-DA-ALL1)
              (* 10
                 (* (fun-expr-arg CLOSE-TO-PI) (* 1 1))))
;(check-expect (eval-all (make-fun-expr 'close-to-nothing 99)
;                        BSL-DA-ALL1)
;              (error NOT-FOUND))

(define (fn-eval-all ex da)
  (local
    (; BSL-fun-expr -> BSL-expr
     ; consumes a fun-expr funx and a definitions area d
     ; and produces the definition of the expresison if its
     ; contained in the definitions area or signals error
     (define (fn-lookup funx d)
       (cond
         [(empty? da) ...]
         [(and (fun-expr? (first d))
               (symbol=? (fun-expr-name funx)
                         (fun-expr-name (first d))))
          (first d)]
         [(and (fun-def? (first d))
               (symbol=? (fun-expr-name funx)
                         (fun-def-name (first d))))
          (first d)]
         [else (fn-lookup funx (rest d))]))

     ; BSL-expr -> BSL-expr
     ; consumes an expression x and produces the expression
     ; with all variables replaced by the function
     ; expressions argument
     (define (fn-lookup-replace x)
       (cond
         [(number? x) ...]
         [(and
           (symbol? x)
           (symbol=? x (fun-def-param (fn-lookup ex da))))
          (fun-expr-arg ex)]
         [(add? x)
          (make-add (fn-lookup-replace (add-left x))
                    (fn-lookup-replace (add-right x)))]
         [(mul? x)
          (make-mul (fn-lookup-replace (mul-left x))
                    (fn-lookup-replace (mul-right x)))]
         [(fun-expr? x) (fun-expr-arg x)]
         [(fun-def? x) (fn-lookup-replace (fn-lookup x da))]
         [else ...])))
    
    (eval-expression (fn-lookup-replace ex))))
          
(define (eval-all ex da)
  (local
    (; BSL-fun-expr -> BSL-expr
     ; consumes a fun-expr funx and a definitions area d
     ; and produces the definition of the expresison if its
     ; contained in the definitions area or signals error
     (define (lookup funx d)
       (cond
         [(empty? d) (error NOT-FOUND)]
         [(and (fun-expr? (first d))
               (symbol=? (fun-expr-name funx)
                         (fun-expr-name (first d))))
          (first d)]
         [(and (fun-def? (first d))
               (symbol=? (fun-expr-name funx)
                         (fun-def-name (first d))))
          (first d)]
         [else (lookup funx (rest d))]))

     ; BSL-expr -> BSL-expr
     ; consumes an expression x and produces the expression
     ; with all variables replaced by the function
     ; expressions argument
     (define (lookup-replace x)
       (cond
         [(number? x) x]
         [(and
           (symbol? x)
           (symbol=? x (fun-def-param (lookup ex da))))
          (fun-expr-arg ex)]
         [(add? x)
          (make-add (lookup-replace (add-left x))
                    (lookup-replace (add-right x)))]
         [(mul? x)
          (make-mul (lookup-replace (mul-left x))
                    (lookup-replace (mul-right x)))]
         [(fun-expr? x) (fun-expr-arg x)]
         [(fun-def? x) (lookup-replace (fun-def-expr x))]
         [else (error NOT-FOUND)])))
    
    (eval-expression (lookup-replace (lookup ex da)))))

;;;;;;;;;;;;;;
;; interpreter

; An S-expr is one of: 
; – Atom
; – SL
	
; An Atom is one of: 
; – Number
; – String
; – Symbol 

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; S-expr Sl -> Number
; consumes an S-expression sexpr and a list of definitions
; sl and produces the value of the expression
(check-expect (interpreter '(lemon 42)
                           '((lemon 99)
                             (orange x (+ x lemon))))
              99)
(check-expect (interpreter '(orange 1)
                           '((lemon 2)
                             (orange x (+ x (lemon 2)))
                             (grapefruit x
                                         (orange x
                                                 (+ x (lemon 2))))))
              (+ 1 2))
(check-expect (interpreter '(grapefruit 10)
                           '((lemon 2)
                             (orange x (+ x (lemon 2)))
                             (grapefruit x
                                         (* (orange x (+ x (lemon 2)))
                                            (lemon 2)))))
              (* (+ 10 2) 2))

(define (interpreter sexpr sl)
  (eval-all (parse sexpr) (map (lambda (x) (parse x)) sl)))