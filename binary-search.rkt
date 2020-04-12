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

(find-root poly -1 8)

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























