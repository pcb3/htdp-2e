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
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

(check-satisfied (round (poly (find-root poly 1 6)))
                 zero?)

;;====
;; 447

(find-root poly 3 5)

