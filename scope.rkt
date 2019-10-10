;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Scope

(require 2htdp/abstraction)

; design enumerate

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index

(check-expect (enumerate '()) '())
(check-expect (enumerate '(a b c))
              (list
               (list 'a 0) (list 'b 1) (list 'c 2)))

(define (fn-enumerate lx)
  (local ((define index
            (... (length lx) (lambda (...) ...))))
    (... ... lx ...)))

(define (enumerate lx)
  (local ((define index
            (build-list (length lx)
                        (lambda (i) i))))
    (map list lx index)))


; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2
(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
  (local (; creates a list of each element in l1
          (define create-list (map list l1))

          ; consumes the second list and extracts the
          ; first element of l2
          (define (extract l)
            (cond
              [(empty? (rest l))
               (add-element (first l)
                            create-list)]
              [else
               (append (add-element (first l)
                                    create-list)
                       (extract (rest l)))]))

          ; adds the elements of l1 to l2
          (define (add-element l ll)
            (cond
              [(empty? ll) '()]
              [else
               (cons (cons l (first ll))
                     (add-element l (rest ll)))])))
    (extract l2)))

;===============

; design and-map and or-map using ISL+ abstractions

; List-of X [X -> Boolean] -> List-of X or #false
; consumes a list and a Boolean producing function
; and produces the list if they all evaluate to true
; or false otherwise

(check-expect (local-and-map even? '(1 2 3)) #false)

(check-expect (local-and-map even? '(0 2 4)) '(0 2 4))

(define (local-and-map f l)
  (local (
          (define (check-items l)
            (cond
              [(empty? l) #true]
              [else (if (f (first l))
                        (check-items (rest l))
                        #false)])))
    (if (check-items l)
        l
        #false)))

; List-of X [X -> Boolean] -> List-of X
; consumes a list and a Boolean producing function
; and produces the a list of all items that
; evaluate to true or false otherwise

(check-expect (or-map even? '(1 2 3)) '(2))

(check-expect (or-map even? '(1 3 5)) #false)

(check-expect (or-map even? '(0 1 2 3)) '(0 2))

(define (fn-or-map f l)
  (local (
          (define (check-items l)
            (...
             [(empty? l) '()]
             [else (... (f (first l))
                        (... (first l)
                             (check-items (rest l)))
                        (check-items (rest l)))])))
    (... (empty? (check-items l))
         ...
         (check-items l))))

(define (or-map f l)
  (local (
          (define (check-items l)
            (cond
              [(empty? l) '()]
              [else (if (f (first l))
                        (cons (first l)
                              (check-items (rest l)))
                        (check-items (rest l)))])))
    (if (empty? (check-items l))
        #false
        (check-items l))))
          
; reformulate all for/... exercises using existing
; abstractions

; .../and

(check-expect ((for-and 10)
               (lambda (x) (> (- 9 x) 0)))
              (for/and ((i 10)) (> (- 9 i) 0)))

(check-expect ((for-and 10)
               (lambda (x) (>= x 0)))
              (for/and ((i 10))
                (if (>= i 0) i #false)))

(define (for-and i)
  (lambda (f)
    (local (
            (define list-builder
              (build-list i (lambda (x) x))))
      
      (if (andmap f list-builder)
          (first (reverse list-builder))
          #false))))

; .../or

;(for/or ([i 10]) (if (= (- 9 i) 0) i #false))
;(for/or ([i 10]) (if (< i 0) i #false))

(define (for-or i)
  (lambda (f)
    (local (
            (define list-builder
              (build-list i (lambda (x) x))))
      
      (if (ormap f list-builder)
          (first (reverse list-builder))
          #false))))

; .../sum

;(for/sum ([c "abc"]) (string->int c))

(define (for-sum i)
  (lambda (f)
    (local (
            (define list-builder
              (cond
                [(string? i)
                 (map f (explode i))]
                [else
                 (build-list i (lambda (x) x))])))
      (foldl + 0 list-builder))))
            

; .../product

;(for/product ([c "abc"]) (+ (string->int c) 1))

; .../string

;(define a (string->int "a"))
;(for/string ([j 10]) (int->string (+ a j)))

;===============

; Exercise 305

; List-of Numbers -> List-of Numbers
; converts a list of US$ amounts to Euros at
; US$1.06 per Euro

(check-expect (convert-euro '(1 2 3))
              (list 1.06 2.12 3.18))

(define (convert-euro l)
  (for/list ((i l)) (* i 1.06)))

; Number -> List-of Numbers
; consumes a natural number n and produces the list
; [0, n-1)

(check-expect (for-nat 3) '(0 1 2))

(define (for-nat n) (for/list ((i n)) i))

; Number -> List-of Numbers
; consumes a natural n and produces the list [0, n]

(check-expect (for-nat-incl 3) '(0 1 2 3))

(define (for-nat-incl n) (for/list ((i (add1 n))) i))

; Number -> List-of Numbers
; consumes a natural n and produces the list
; [1, 1/2, ..., 1/n]

(check-expect (1/n 1) '(1))
(check-expect (1/n 3) (list 1 (/ 1 2) (/ 1 3)))

(define (fn-1/n n)
  (for/list [(... (... ... n ...))]
    (... ... ...)))

(define (1/n n)
  (for/list [(item (in-range 1 (add1 n) 1))]
    (/ 1 item)))

; Number -> List-of Numbers
; consumes a natural n and produces a list of the
; first n evens

(check-expect (first-n 1) (list 0))
(check-expect (first-n 3) (list 0 2 4))

(define (first-n n)
  (for/list [(i n)] (* 2 i)))

; Number -> List List-of Numbers
; consumes a natural n and produces the n identity
; matrix

(check-expect (for-identity 1) (list (list 1)))

(check-expect (for-identity 3) (list (list 1 0 0)
                                     (list 0 1 0)
                                     (list 0 0 1)))

(define (fn-for-identity n)
  (for/list [(... n)]
    (list (for/list [(... n)]
            (... ...)))))

(define (for-identity n)
  (for/list [(i n)]
    (for/list [(j n)]
      (if (= i j) 1 0))))

; Number -> [List-of Number]
; consumes a number n and a function f and
; produces a list of [0, n] apllied to f

(define (fn-for-tab n f)
  (for/list [(... n)] (f ...)))

(define (for-tab n f)
  (for/list [(i (add1 n))] (f i)))
          
; Exercise 307

; String List-of String -> String
; consumes a name s and a list of names los and
; produces the first name equal to or an extension
; of the name s

(check-expect
 (find-name "seifer" (list "cloud"
                           "selphie"
                           "seifer")) "seifer")

(check-expect
 (find-name "seifer" (list "cloud"
                           "seiferb"
                           "selphie"
                           "seifer")) "seiferb")

(define (fn-find-name s los)
  (for/list [(i s) (j (first los))]
    (... (... i j)
         (first los)
         (fn-find-name s (rest los)))))
         
(define (find-name s los)
  (cond
    [(empty? los) #false]
    [else
     (if
      (for/and [(i s) (j (first los))]
        (and (string=? i j)
             (>= (string-length (first los))
                 (string-length s))))
      (first los)
      (find-name s (rest los)))]))
















































