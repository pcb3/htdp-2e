;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname making-choices) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 26.4 Making Choices

(require plot)

(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

;;====
;; 439

;(time (gcd-structural 101135853 45014640))

;;====
;; 438

; the minimum natural of n and m is and used as the input
; in the local function. In the trivial case if i is equal to 1,
; 1 is produced. Else, if i is a divisor of both n and m, then
; the gcd is i. If not then the local function is called again
; with i - 1.

; Geatest-diviso-<= recurs on (min n m) because this is the
; step that brings the funciton closer to termination and the
; gcd of any two numbers must be at less than or equal to
; (min n m)

(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

;;====
;; 440

;(time (gcd-generative 101135853 45014640))

;;================================
;; 441

;; 14 recursive calls to quicksort
;; 7 recursive calls to append

;;====
;; 442

;;=====
;; sort

(define (sort< l)
  (cond
    [(empty? l) '()]
    [else (insert (first l) (sort< (rest l)))]))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (< n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

;;============
;; quick-sort<

(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon))
                  (define lop
                    (filter (lambda (x) (= pivot x)) alon)))
            (append (quick-sort< (smallers alon pivot))
                    lop
                    (quick-sort< (largers alon pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

;(sort< '())
;(sort< '(1 1 1 1))
;(sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;(quick-sort< '())
;(quick-sort< '(1 1 1 1))
;(quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))

; N N N -> [List-of [List-of Number]]
; consumes a Natural n and a Natural d and
; produces a list of depth d of lists of increasing length up
; to length d of random numbers [0, n)
(check-random (create-tests 100 3)
              (build-list
               3
               (lambda (a)
                 (build-list
                  (expt 2 a) (lambda (m) (random 100))))))

(define (fn-create-tests n d)
  (build-list
   d (lambda (a)
       (build-list (... a)
                   (lambda (b) (... n))))))

(define (create-tests n d)
  (build-list
   d (lambda (a) (build-list (expt 2 a)
                             (lambda (b) (random n))))))

(define TEST-SUITE (create-tests 100 10))
;
;(map (lambda (a) (time (sort< a))) TEST-SUITE)
;(map (lambda (a) (time (quick-sort< a))) TEST-SUITE)

;;============
;; clever-sort

(define THRESHOLD 7)

; [List-of Number] -> [List-of Number]
; consumes a list of numbers alon and produces an increasingly
; sorted list using quicksort< for large lists and sort<
; otherwise.
(check-expect (clever-sort '()) '())
(check-expect (clever-sort '(2)) '(2))
(check-expect (clever-sort '(3 2 1)) '(1 2 3))
(check-random (clever-sort
               (build-list 100 (lambda (x) (random 100))))
              (sort<
               (build-list 100 (lambda (x) (random 100)))))

(define (clever-sort alon)
  (if (> (length alon) THRESHOLD)
      (quick-sort< alon)
      (sort< alon)))

;;====
;; 443

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m

;; ???

;;====
;; 444

; divisors consumes two numbers becuse you want the funciton
; to terminate and so can recur on one of the numbers

; the gcd of both must be smaller or equal to the
; smallest number. The choice of numbers is irrelevant as long
; as you have an exhaustive list of all divisors of L <= S
; and S <= S or vice-versa


























