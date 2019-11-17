;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 19.3 S-expressions

(require 2htdp/abstraction)

; An S-expr is one of: 
; – Atom
; – SL

; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol

; Exercise 316

; X -> Boolean
; consumes an object x and produces true if it is
; either a number, string or symbol

(check-expect (atom? 1) #true)
(check-expect (atom? "hi there") #true)
(check-expect (atom? '$) #true)
(check-expect (atom? '()) #false)

(define (fn-atom? x)
  (or (number? ...)
      (string? ...)
      (symbol? ...)))

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))

; S-expr Symbol -> N
; counts all occurrences of sy in sexp

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect
 (count '(((world) hello) hello) 'hello) 2)

(define (fn-count sexp sy)
  (cond
    [(atom? sexp)
     (fn-count-atom sexp sy)]
    [else
     (fn-count-sl sexp sy)]))

(define (fn-count-sl sl sy)
  (cond
    [(empty? sl) ...]
    [else
     (...
      (fn-count (first sl) sy)
      ...
      (fn-count-sl (rest sl) sy)
      ...)]))

(define (fn-count-atom at sy)
  (cond
    [(number? at) ...]
    [(string? at) ...]
    [(symbol? at) ...]))

; Exercise 317

(define (count sexp sy)
  (local
    (; SL -> N
     ; counts all occurrences of sy in sl
     (define (count-sl sl)
       (cond
         [(empty? sl) 0]
         [else
          (+ (count (first sl) sy)
             (count-sl (rest sl)))]))

     ; Atom -> N
     ; counts all occurrences of sy in at
     (define (count-atom at)
       (cond
         [(number? at) 0]
         [(string? at) 0]
         [(symbol? at)
          (if (symbol=? at sy) 1 0)])))

    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))

; Exercise 318

; S-expr -> Number
; consumes an S-expression sexp and determines its
; depth. An atom has depth 1 and a list of an atom
; has depth 1 plus 1

(check-expect (depth 99) 1)
(check-expect (depth (list "abc" "cba")) 2)
(check-expect (depth (list
                      (list '@))) 3)
(check-expect (depth (list
                      (list
                       (list "sup")))) 4)

(define (fn-depth sexp)
  (cond
    [(atom? sexp) ...]
    [else
     (fn-traverse sexp)]))
         
(define (fn-traverse sl)
  (cond
    [(empty? sl) ...]
    [else
     (... (fn-depth (first sl))
          ...
          (fn-traverse (rest sl)))]))

(define (depth sexp)
  (local
    (; S-expr -> Number
     ; consumes and S-expression and produces the
     ; depth
     (define (traverse-sl sl)
       (cond
         [(empty? sl) 1]
         [else
          (max (depth (first sl))
               (traverse-sl (rest sl)))])))
    
    (cond
      [(atom? sexp) 1]
      [else
       (add1 (traverse-sl sexp))])))

; Exercise 319

; S-expr Symbol Symbol -> S-expr
; consumes an S-expression s and two symbols old
; and new and creates a new S-expr with old replaced
; by new

(check-expect (substitute '() '$ '@) '())

(check-expect (substitute (list '$) '$ '@) (list '@))

(check-expect (substitute
               (list '*) '$ '@) (list '*))

(check-expect (substitute
               (list '*
                     (list '* '$
                           (list '* '$)))
               '$ '@)
              (list '*
                    (list '* '@
                          (list '* '@))))

(define (fn-substitute s old new)
  (cond
    [(atom? s)
     (fn-symbol s ... ...)]
    [else
     (fn-traverse-sub-sl (... s) ... ...)]))

(define (fn-symbol sym old new)
  (cond
    [else (if (... (symbol? sym)
                   (symbol=? sym old))
              ... ...)]))         

(define (fn-traverse-sub-sl sl old new)
  (cond
    [(empty? sl) ...]
    [else
     (... (fn-substitute (first sl) ... ...)
          (... (fn-traverse-sub-sl (rest sl)
                                   ... ...)))]))

(define (substitute s old new)
  (local
    ((define (traverse-sub-sl sl)
       (cond
         [(empty? sl) '()]
         [else
          (cons (substitute (first sl) old new)
                (traverse-sub-sl (rest sl)))]))

     (define (symbol sym)
       (cond
         [else
          (if (and (symbol? sym)
                   (symbol=? sym old))
              new sym)])))

    (cond
      [(atom? s) (symbol s)]
      [else
       (traverse-sub-sl s)])))
     
; Exercise 320

; An S-expr is one of: 
; – Number
; - String
; - Symbol
; – [List-of S-expr]

(define SEXP1 1)
(define SEXP2 "two")
(define SEXP3 '$)
(define SEXP4 '())
(define SEXP5 (list SEXP1 SEXP2 SEXP3 SEXP4))

; S-expr Symbol -> Number
; counts the number of times a symbol sy appears
; in an given S-expression sexp

(check-expect (count-again '() '$) 0)
(check-expect (count-again 33 '$) 0)
(check-expect (count-again '@ '$) 0)
(check-expect (count-again '$ '$) 1)
(check-expect (count-again (list '$
                                 (list '@ '$))'$) 2)

(define (fn-count-again sexp sy)
  (cond
    [(atom? sexp)
     (fn-count-symbol sexp sy)]
    [else
     (fn-traverse-list sexp sy)]))

(define (fn-traverse-list sl sy)
  (cond
    [(empty? sl) ...]
    [else
     (...
      (fn-count-again (first sl))
      ...
      (fn-traverse-list (rest sl) sy)
      ...)]))

(define (fn-count-symbol at sy)
  (cond
    [(number? at) ...]
    [(string? at) ...]
    [else
     (... (... at sy) ...)]))

(define (count-again sexp sy)
  (cond
    [(atom? sexp)
     (count-symbol sexp sy)]
    [else
     (traverse-list sexp sy)]))

(define (traverse-list sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+
      (count-again (first sl) sy)
      (traverse-list (rest sl) sy))]))

(define (count-symbol at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [else
     (if (symbol=? at sy) 1 0)]))

; An S-expr is one of:
; '()
; – Number
; - String
; - Symbol
; – [NE-List-of sl]

(define (count-v2 sexp sy)
  (local
    ((define (traverse-sexp s)
       (cond
         [(empty? s) 0]
         [else
          (+ (count-v2 (first s) sy)
             (traverse-sexp (rest s)))]))

     (define (count-sy at)
       (cond
         [(not (symbol? at)) 0]
         [else
          (if (symbol=? at sy) 1 0)])))

    (cond
      [(atom? sexp)
       (count-sy sexp)]
      [else
       (traverse-sexp sexp)])))
      
; Implementation by adaliu-gh
; https://github.com/adaliu-gh/htdp

(define (count.v3 sexp sy)
  (cond
    [(empty? sexp) 0]
    [(list? sexp)
     (foldl
      (lambda (x y) (+ (count.v3 x sy) y)) 0 sexp)]
    [else (if (equal? sexp sy) 1 0)]))

(count.v3 (list '(world pig) 1 'world) 'world)
(count.v3 (list (list (list 'world))) 'world)
          
; an Atom is:
; - Any item

; count-v3

(define (count-v3 sexp sy)
  (local
    ((define (traverse-sexp s)
       (cond
         [(empty? s) 0]
         [else
          (+ (count-v3 (first s) sy)
             (traverse-sexp (rest s)))])))

    (cond
      [(atom? sexp)
       ((lambda (at)
         (if (equal? at sy) 1 0)) sexp)]
      [else
       (traverse-sexp sexp)])))
          
         
         























