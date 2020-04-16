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
(check-expect
 (find-linear (make-table 3 (lambda (x) (- 1 x)))) 1)
(check-expect
 (find-linear (make-table 3 (lambda (x) x))) 0)

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


;;===============
;; find-linear.v2

(check-expect
 (find-linear.v2 (make-table 3 (lambda (x) (- 1 x)))) 1)
(check-expect
 (find-linear.v2 (make-table 3 (lambda (x) x))) 0)

(define (find-linear.v2 t)
  (local
    (
     (define table-size (table-length t))

     (define array-value
       (map (lambda (j)
              (if (negative? j)
                  (* -1 j) j))
            (map (lambda (x) (table-ref t x))
                 (build-list table-size (lambda (y) y)))))

     (define (find-root n index root arr)
       (cond
         [(empty? arr) index]
         [else
          (if (<= root (first arr))
              (find-root (add1 n) index root (rest arr))
              (find-root (add1 n) n (first arr) (rest arr)))])))
    
    (cond
      [(= table-size 1) 0]
      [else
       (find-root 1 0 (first array-value)
                  (rest array-value))])))

;;============
;; find-binary

; Table -> N
; consumes a table t and finds the smallest index for a root of
; the table
; termination argument:
; assume f is monotonically increasing
; generative step finds median of search space if size is odd,
; or if even divides by 2. Takes index on either side and if
; it is closer to 0 or 0 then outputs that index else repeats
; the process on the value of the index closest to 0
(check-expect
 (find-binary (make-table 3 (lambda (x) (- 1 x)))) 1)
(check-expect
 (find-binary (make-table 4 (lambda (x) (- 1 x)))) 1)
(check-expect
 (find-binary (make-table 3 (lambda (x) x))) 0)
(check-expect
 (find-binary (make-table 3 (lambda (x) 24))) 0)

(define (fn-find-binary t)
  (local
    (

     (define max-index ...)
     (define fl ...)
     (define fr ...)
     (define fmid ...)
     
     (define (fn-helper l r)
       (cond
         [(= l max-index) ...]
         [(and (= l 0) (<= fl fr)) ...]
         [(<= fl fmid) (fn-helper 0 l)]
         [(< fr fmid) (fn-helper r max-index)]
         [else ...])))

    (fn-helper 0 max-index)))
         
(define (find-binary t)
  (local
    (
     (define (neg->pos fi)
       (cond
         [(zero? fi) 0]
         [else (if (negative? fi) (* -1 fi) fi)]))
     
     (define max-index (sub1 (table-length t)))

     (define (helper l r)
       (local
         (
          (define mid (round (/ (+ l r) 2)))
          (define fmid (neg->pos (table-ref t mid)))
          (define fl (neg->pos (table-ref t l)))
          (define fr (neg->pos (table-ref t r))))

         (cond
           [(= l max-index) r]
           [(and (= l 0) (<= fl fr) (= l (sub1 r))) l]
           [(<= fl fmid) (helper l mid)]
           [(< fr fmid) (helper mid r)]
           [else mid]))))

    (helper 0 max-index))) 
       



























