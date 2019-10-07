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




















