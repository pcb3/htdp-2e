;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intermezzo4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Intermezzo 4: The Nature of Numbers


;;=============================
;; Fixed-Size Number Arithmetic

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)
; An S is one of:
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99) (<= 0 e 99) (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad values given")]))
 
; Inex -> Number
; converts an inex into its numeric equivalent 
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt
      10 (* (inex-sign an-inex) (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

;;====
;; 412

; Inex Inex -> Inex
; consumes two Inex and adds them together
(check-expect
 (inex+ (create-inex 1 1 0) (create-inex 2 1 0))
 (create-inex 3 1 0))

; integer division by 10 and adding 1 to exponent if
; mantissa > 99
(check-expect
 (inex+ (create-inex 55 1 0) (create-inex 55 1 0))
 (create-inex 11 1 1))

; rounding to nearest inex
(check-expect
 (inex+ (create-inex 56 1 0) (create-inex 56 1 0))
 (create-inex 11 1 1))

; challenge: handle exponents that differ by 1
;(check-expect
; (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
; (create-inex 11 -1 1))

(check-error
 (inex+ MAX-POSITIVE MAX-POSITIVE)
 "result out of range")

(define (fn-inex+ inex1 inex2)
  (local
    (
     ; Inex Inex -> Number
     ; raw number by adding both mantissi
     (define raw-add
       (+ (inex-mantissa ...) (inex-mantissa ...)))
     
     ; Inex Inex -> Inex
     ; create a new inex from two inex representations
     (define build-inex
       (cond
         [else
          (if (> raw-add ...)
              (create-inex
               (round (/ raw-add ...))
               (inex-sign ...) (add1 (inex-exponent ...)))
              (create-inex raw-add
                           (inex-sign ...)
                           (inex-exponent ...)))])))

    build-inex))

(define (inex+ inex1 inex2)
  (local
    (
     (define mant1 (inex-mantissa inex1))
     (define mant2 (inex-mantissa inex2))
     (define sign1 (inex-sign inex1))
     (define sign2 (inex-sign inex2))
     (define expt1 (inex-exponent inex1))
     (define expt2 (inex-exponent inex2))
     
     ; builds a temporary inex
     (define raw-inex
       (make-inex (+ (inex-mantissa inex1)
                     (inex-mantissa inex2))
                  (inex-sign inex1)
                  (inex-exponent inex1)))
     
     ; checks that Inex is within range
     (define exceeds-range?
       (or (> (+ (inex->number inex1)
                 (inex->number inex2))
              (inex->number MAX-POSITIVE))
           (and (> (+ (inex-mantissa inex1)
                      (inex-mantissa inex2)) 99)
                (or (> (inex-exponent inex1) 99)
                    (> (inex-exponent inex2) 99)))))

     ; constructs the sum of two inex
     (define (build-inex inx)
       (cond
         [(> (inex-mantissa inx) (inex-mantissa MAX-POSITIVE))
          (build-inex
           (make-inex (/ (inex-mantissa inx)  10)
                      (inex-sign inx)
                      (add1 (inex-exponent inx))))]
         [(> (inex-exponent inx) (inex-exponent MAX-POSITIVE))
          (error "result out of range")]
         [else (create-inex (round (inex-mantissa inx))
                            (inex-sign inx)
                            (inex-exponent inx))])))
    
    (build-inex raw-inex)))

;;====
;; 413

; Inex Inex -> Inex
; multiplies two Inex's together
(check-expect
 (inex* (create-inex 2 1 0) (create-inex 3 1 0))
 (create-inex 6 1 0))
(check-expect
 (inex* (create-inex 2 1 5) (create-inex 2 1 5))
 (create-inex 4 1 10))
(check-expect
 (inex* (create-inex 2 -1 3) (create-inex 2 -1 2))
 (create-inex 4 -1 5))
(check-error
 (inex* MAX-POSITIVE MAX-POSITIVE)
 "result out of range")
(check-error
 (inex* MIN-POSITIVE MIN-POSITIVE)
 "result out of range")

(define (fn-inex* inex1 inex2)
  (local

    ((define mant1 (inex-mantissa inex1))
     (define mant2 (inex-mantissa inex2))
     (define sign1 (inex-sign inex1))
     (define sign2 (inex-sign inex2))
     (define expt1 (inex-exponent inex1))
     (define expt2 (inex-exponent inex2))
     (define maxmant (inex-mantissa MAX-POSITIVE))
     (define minmant (inex-mantissa MIN-POSITIVE))
     (define maxexpt (inex-exponent MAX-POSITIVE))
     (define minexpt (inex-exponent MIN-POSITIVE))

     (define check-positive
       (positive? (+ (* sign1 expt1) (* sign2 expt2))))
             
     (define raw-inex
       (make-inex
        (* mant1 mant2)
        (cond (else (if check-positive 1 -1)))
        (+ (* sign1 expt1) (* sign2 expt2))))

     (define (reduce-to-range raw)
       (cond
         [(> (inex->number raw) (inex->number MAX-POSITIVE))
          (error "result out of range")]
         [(< (inex->number raw) (inex->number MIN-POSITIVE))
          (error "result out of range")]
         [(> (round (inex-mantissa raw)) maxmant)
          (reduce-to-range
           (make-inex (/ (inex-mantissa raw) 10)
                      (inex-sign raw)
                      (add1 (inex-exponent raw))))]
         [(< (round (inex-mantissa raw)) 1)
          (reduce-to-range
           (make-inex (* (inex-mantissa raw) 10)
                      (inex-sign raw)
                      (sub1 (inex-exponent raw))))]
         [else (make-inex (round (inex-mantissa raw))
                          (inex-sign raw)
                          (inex-exponent raw))])))

    (create-inex (reduce-to-range raw-inex))))
                                    
(define (inex* inex1 inex2)
  (local

    ((define mant1 (inex-mantissa inex1))
     (define mant2 (inex-mantissa inex2))
     (define sign1 (inex-sign inex1))
     (define sign2 (inex-sign inex2))
     (define expt1 (inex-exponent inex1))
     (define expt2 (inex-exponent inex2))
     (define maxmant (inex-mantissa MAX-POSITIVE))
     (define minmant (inex-mantissa MIN-POSITIVE))
     (define maxexpt (inex-exponent MAX-POSITIVE))
     (define minexpt (inex-exponent MIN-POSITIVE))
     (define sum-exponents (+ (* sign1 expt1) (* sign2 expt2)))
             
     (define raw-inex
       (make-inex
        (* mant1 mant2)
        (cond
          [(positive? sum-exponents) 1]
          [(negative? sum-exponents) -1]
          [(zero? sum-exponents) 1])
        (cond
          [else (if (negative? sum-exponents)
                    (* -1 sum-exponents) sum-exponents)])))

     (define (reduce-to-range raw)
       (cond
         [(> (inex->number raw) (inex->number MAX-POSITIVE))
          (error "result out of range")]
         [(< (inex->number raw) (inex->number MIN-POSITIVE))
          (error "result out of range")]
         [(> (round (inex-mantissa raw)) maxmant)
          (reduce-to-range
           (make-inex (/ (inex-mantissa raw) 10)
                      (inex-sign raw)
                      (add1 (inex-exponent raw))))]
         [(< (round (inex-mantissa raw)) 1)
          (reduce-to-range
           (make-inex (* (inex-mantissa raw) 10)
                      (inex-sign raw)
                      (sub1 (inex-exponent raw))))]
         [else (make-inex (round (inex-mantissa raw))
                          (inex-sign raw)
                          (inex-exponent raw))]))

     (define refined-inex (reduce-to-range raw-inex)))

    (create-inex (round (inex-mantissa refined-inex))
                 (inex-sign refined-inex)
                 (inex-exponent refined-inex))))

;;====
;; 414

; Number -> Number
; consumes a number n and adds up n many copies of #i1/185

(check-within (add 1) #i1/185 0.0001)
(check-within (add 185) 1 0.0001)

(define (fn-add n)
  (cond
    [(zero? n) ...]
    [else (... ... (fn-add (sub1 n)))]))

(define (add n)
  (cond
    [(zero? n) 0]
    [else (+ #i1/185 (add (sub1 n)))]))

; Number -> Number
; consumes a number n and a counter i and subtracts n copies
; of 1/185 until n is equal to 0 and produces the number of
; times this was possible
(check-expect (sub 1 0) 185)
; (check-expect (sub #i1.0 0) 185)
; condition never met so function never halts

(define (fn-sub n i)
  (cond
    [(zero? n) ...]
    [else (fn-sub (- ... ...) (add1 ...))]))

(define (sub n i)
  (cond
    [(zero? n) i]
    [else (sub (- n 1/185) (add1 i))]))
    
 







































