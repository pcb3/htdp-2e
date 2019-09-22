;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname specifying-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 17.4 Specifying with lambda

; exercise 292

; [X X -> Boolean] [NEList-of X] -> Booelan
; determines whether l is sorted according to cmp

(check-expect (sorted? < '(1 2 3)) #true)
(check-expect (sorted? < '(2 1 3)) #false)

(define (fn-sorted? cmp l)
  (cond
    [(empty? (rest l)) ...]
    [else (if (cmp (... l) (... (... l)))
              (fn-sorted? cmp (rest l))
              ...)]))

(define (sorted? cmp l)
  (cond
    [(empty? (rest l)) #true]
    [else (if (cmp (first l) (first (rest l)))
              (sorted? cmp (rest l))
              #false)]))

; redefine sorted to use sorted?


; [X X -> Boolean] -> [[List-of X] -> Boolean]
; is the given list l0 sorted according to cmp

(define (sorted cmp)
  (lambda (l0)
    (if (empty? l0) #true (sorted? cmp l0))))

; exercise 293

; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))


; X [List-of X] -> [X X -> Boolean] -> [List-of X List-of X -> Boolean]
; checks that the empty list l is a sublist of itself
; checks that the sublist is a member of l
; checks that the sublist is equal to the sublist of l from x

;(check-expect [(found? "a" '()) '()] #true)
(check-expect [(found? "b" '("a" "b" "c")) '("b" "c")]
              #true)
(check-expect [(found? "b" '("a" "b" "c")) '("c")]
              #false)

(define (fn-found? 1str l) #true)

(define (found? 1str l)
  (lambda (l0)
    (local (; produces true if the all element in the sublist
            ; are members of l
            (define contains?
              (andmap (lambda (in-l) (member? in-l l)) l0))

            ; produces the sublist of l from x
            (define (sublist lx)
              (cond
                [(empty? l) #false]
                [(equal? (first lx) 1str) lx]
                [else
                 (sublist (rest lx))]))

            ; produces true if the sublist of l is equal to
            ; the sublist l0
            (define sublist-eq?
              (equal? (sublist l) l0)))

      (and sublist-eq?
           contains?))))

(check-satisfied (find "a" '("b" "a" "c"))
                 (found? "a" '("b" "a" "c")))























