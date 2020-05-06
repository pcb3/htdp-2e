;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gaussian-elimination) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 28.3 Gaussian Elimination

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

;;====
;; 462

; SOE Solution -> Boolean
; consumes an SOE soe and a Solution sol and produces #true if
; both sides of the equation are equal after plugging in values
; of variables for the entire system

(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(0 0 1)) #false)

(define (check-solution soe sol)
  (andmap (lambda (x) (plug-in x sol)) soe))
  
; Equation Solution -> Boolean
; consumes an Equation equ and a Solution sol and produces
; #true when the left-hand-side equal the right-hand-side
; when the solution set is substituted into the variables

(check-expect (plug-in (first M) S) #t)
(check-expect (plug-in (first M) '(0 0 0)) #f)

(define (plug-in equ sol)
  (equal?
   (foldl + 0 (map (lambda (a b) (* a b)) (lhs equ) sol))
   (rhs equ)))
              
;;====
;; 462

(define triangle-M '((2 2 3 10)
                     (0 3 9 21)
                     (0 0 1 2)))

(check-expect (check-solution triangle-M S) #t)

(define transformed-M '((2 2 3 10)
                        (0 3 9 21)
                        (0 -3 -8 -19)))

(check-expect (check-solution transformed-M S) #t)

;;====
;; 465

; Equation Equation -> Equation
; consumes two Equations eq1 and eq2 and produces the rest of
; an Equation that has a a zero in the leading coefficient

(check-expect (subtract '(1 2 3 8) '(2 3 1 10))
              '(-1 -5 -6))
(check-expect (subtract '(2 2 1 5) '(1 3 4 2))
              '(2 3.5 -0.5))
(check-expect (subtract '(1 1 1 2) '(-1 -1 -1 4))
              '(0 0 6))

(define (subtract eq1 eq2)
  (rest
   (map (lambda (x y)
          (- y (* (/ (first eq2) (first eq1)) x))) eq1 eq2)))

;;====
;; 466

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations 
; generation: uses subtract on an Equation to create a zero
; in the leading coefficient, reducing its length.
; termination:

(check-expect (triangulate (list (list 1 2 3 4)))
              (list (list 1 2 3 4)))
                                 
(check-expect (triangulate (list (list 1 2 3 4)
                                 (list 0 0 2 2)))
              (list (list 1 2 3 4)
                    (list 0 2 2)))

(check-expect (triangulate
               (list (list 2 2 5 10)
                     (list 1 1 2 4)))
              (list (list 2 2 5 10)
                    (list 0 -0.5 -1)))

(check-expect (triangulate
               (list (list 2  2  3  10)
                     (list 0  3  9  21)
                     (list 0 -3 -8 -19)))
              (list (list 2  2  3  10)
                    (list 3  9 21)
                    (list    1  2)))

(check-expect (triangulate
               (list (list   0 -5  -5)
                     (list  -8 -4 -12)))
              (list (list   -8 -4 -12)
                    (list      -5  -5)))

(define (triangulate soe)
  (cond
    [(empty? (rest soe)) soe]
    [(zero? (first (first soe)))
     (triangulate (append (rest soe)
                          (list (first soe))))]
    [else
     (cons
      (first soe)
      (triangulate
       (map
        (lambda (x) (subtract (first soe) x))
        (rest soe))))]))
    































