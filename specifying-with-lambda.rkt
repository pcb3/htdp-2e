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


; X [List-of X] -> [List-of X List-of X -> Boolean]
; checks that the empty list l is a sublist of itself
; checks that the sublist is a member of l
; checks that the sublist is equal to the sublist of l from x

(check-expect [(found? "a" '()) '()] #true)
(check-expect [(found? "b" '("a" "b" "c")) '("b" "c")]
              #true)
(check-expect [(found? "b" '("a" "b" "c")) '("c")]
              #false)

(define (fn-found? 1str l) #true)

(define (found? 1str l)
  (lambda (l0)
    (local (; produces true if the all element in the sublist
            ; are members of l

            ; this function is now covered by sublist-eq?
            ; but left as an exemplar of currying
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

      (or (and (empty? l)
               (empty? l0))
          (and sublist-eq?
           contains?)))))

(check-satisfied (find "a" '("b" "a" "c"))
                 (found? "a" '("b" "a" "c")))

; exercise 294

; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

(check-satisfied (index "a" '("a"))
                 (is-index? "a" '("a")))
(check-satisfied (index "a" '("a" "b" "c"))
                 (is-index? "a" '("a")))

; X [List-of X] -> [Number -> Boolean]
; is the index equal to x in l
; is the index of an x not in l false

(check-expect [(is-index? "a" '("a")) 0] #true)
(check-expect [(is-index? "a" '( "x" "a")) 0] #false)
(check-expect [(is-index? "a" '("b" "a" "a")) 1] #true)

(define (is-index? x l)
  (lambda (dex)
    (local (; produces true if x is in l
            ; and false otherwise
            (define x-in-l?
              (member? x l))

            ; finds the index of x in l
            (define (x-index l i)
              (cond
                [(empty? l) #false]
                [(equal? (first l) x) i]
                [else
                 (x-index (rest l) (add1 i))]))

            ; compares x-index and dex for equality
            (define compare
              (eq? (x-index l 0) dex)))

      (and x-in-l?
           compare))))

; exercise 295

; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

; Number -> [list-of Posn] -> Boolean]]
; consumes a Number count and a list of Posns and
; checks if:
; the length of the given list is equal to the count
; all Posns are within the WIDTH by HEIGHT rectangle

(check-expect [(n-inside-playground? 1)
               (list (make-posn 0 0))]
              #true)
(check-expect [(n-inside-playground? 3)
               (list (make-posn 0 0)
                     (make-posn 400 -20)
                     (make-posn 0 0))]
              #false)

(define (fn-n-inside-playground? n)
  (lambda (lop)
    (local (
            (define n-length-eq?
              (... n (... lop)))

            (define (check-width pos)
              (... (... (posn-x pos) ...)
                   (... (posn-x pos) ...)))

            (define (check-height pos)
              (... (... (posn-y pos) ...)
                   (... (posn-y pos) ...))))
      
      (... n-length-eq?
           (... check-width ...)
           (... check-height ...)))))

(define (n-inside-playground? n)
  (lambda (lop)
    (local (
            (define n-length-eq?
              (eq? n (length lop)))

            (define (check-width pos)
              (and (< (posn-x pos) WIDTH)
                   (>= (posn-x pos) 0)))

            (define (check-height pos)
              (and (< (posn-y pos) HEIGHT)
                   (>= (posn-y pos) 0))))
      
      (and n-length-eq?
           (andmap check-width lop)
           (andmap check-height lop)))))


























