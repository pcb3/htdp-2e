;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 19.3 S-expressions

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

; Exercise 318

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
     


     
     























