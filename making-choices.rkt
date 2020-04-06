;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname making-choices) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 26.4 Making Choices

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

(time (gcd-structural 101135853 45014640))

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

(time (gcd-generative 101135853 45014640))




























