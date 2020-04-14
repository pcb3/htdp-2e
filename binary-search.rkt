;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binary-search) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 27.2 Binary Search

;;====
;; 445

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

(define ε 0.00000001)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
(check-satisfied (round (poly (find-root poly 1 6)))
                 zero?)

(define (find-root f left right)
  (local ((define (helper l r fl fr)
            (cond
              [(<= (- r l) ε) l]
              [else
               (local ((define mid (/ (+ l r) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(or (<= fl 0 f@mid) (<= f@mid 0 fl))
                    (helper l mid fl f@mid)]
                   [(or (<= f@mid 0 fr) (<= fr 0 f@mid))
                    (helper mid r f@mid fr)]))])))
    (helper left right (f left) (f right))))

;;====
;; 447

(check-expect (round (find-root poly -1 8)) 2)

;;====
;; 448

; The assumption requires f left and f right to be on opposite
; sides of the x-axis. If that is true, then we know that root
; lies between those two points as it is defined as f(x) = 0.
; And because the funciton is continous there must be some value
; of x that outputs 0 and therfore the funciton must terminate.

; termination argument:
; at each step of the algorithm the search space is divided by
; 1/2 and so we are closer to within epsilon of the root.

;;====
;; 450

(check-satisfied (round (poly (find-root.v2 poly 1 6)))
                 zero?)

(define (find-root.v2 f left right)
  (local ((define (helper l r fl fr)
            (cond
              [(<= (- r l) ε) l]
              [else
               (local ((define mid (/ (+ l r) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(<= fl 0 f@mid)
                    (helper l mid fl f@mid)]
                   [(<= f@mid 0 fr)
                    (helper mid r f@mid fr)]))])))
    (helper left right (f left) (f right))))

;;====
;; 451

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (lambda (i) i)))

; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= 0")))

(define table2 (make-table 1 a2))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> N
; consumes a monotonically increasing table t and produces
; the smallest index for a root of the table

(define (fn-find-linear t)
  (local
    ((define table-size (table-length t))

     (define initial-root (table-ref t 0))

     (define (iterate n index root)
       (cond
         [(= n table-size) ...]
         [else
          (if
           (< root
              (cond
                [(negative? (table-ref t n))
                 (* -1 (table-ref t n))]
                [else (table-ref t n)]))
           (iterate (add1 n) n (table-ref t n))
           (iterate (add1 n) index root))])))
    (cond
      [(= table-length 1) ...]
      [else (iterate 1 0 initial-root)])))

(define (find-linear t)
  (local
    ((define table-size (table-length t))

     (define initial-root (table-ref t 0))

     (define (iterate n index root)
       (cond
         [(= n table-size) index]
         [else
          (if
           (<= root
              (cond
                [(negative? (table-ref t n))
                 (* -1 (table-ref t n))]
                [else (table-ref t n)]))
           (iterate (add1 n) index root)
           (iterate (add1 n) n (table-ref t n)))])))
    
    (cond
      [(= table-size 1) 0]
      [else (iterate 1 0 initial-root)])))





























