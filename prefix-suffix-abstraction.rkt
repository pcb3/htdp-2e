;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prefix-suffix-abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 274
; use existing abstractions to define prefixes and
; suffixes functions

; List-of-1Strings -> List-of-List-of-1Strings
; consumes a list of 1Strings and produces all
; prefixes of that list using an abstraction

;(check-expect (prefix '()) '())
;(check-expect (prefix '("a" "b" "c"))
;              (list '("a" "b" "c")
;                    '("a" "b")
;                    '("a")))

;(define (prefix lo1)
;  (local (
;          (define (helper l)
;            (cond
;              [(empty? l) '()]
;              [else
;               (cons
;                (drop-last l)
;                (helper
;                 (reverse (rest (reverse l)))))]))
;          ; List-of X -> List-of X
;          ; ...
;          (define (fn-drop-last lx)
;            (cond
;              [(empty? (rest lx)) ...]
;              [else
;               (... (first lx)
;                    (fn-drop-last (rest lx)))]))
;          
;          (define (drop-last lx)
;            (cond
;              [(empty? (rest lx)) '()]
;              [else
;               (cons (first lx)
;                     (drop-last (rest lx)))])))
;    
;    ()))

(define (prefix-builder lo1)
  (local (; List-of-1Strings -> List-of-1Strings
          (define (builder l)
            (cond
              [(empty? l) '()]
              [else
               (cons l
                     (builder (rest l)))])))
    (builder (reverse lo1))))

(define (prefix-counter lo1)
  (local (; List-of-Lo1 -> List-of-Lo1
          ; consumes a list of 1Strings and produces
          ; a list of prefixes for n items in the lo1
          (define (helper lx nl)
            (cond
              [(zero? nl) '()]
              [else
               (cons (first lx)
                     (helper (rest lx) (sub1 nl)))]))
          (define (builder ll nn)
            (cond
              [(zero? nn) '()]
              [else
               (cons (helper lo1 nn)
                     (builder ll (sub1 nn)))])))
    (builder lo1 (length lo1))))
   
(prefix-counter '(1 2 3))

; [ X ] [ List-of X ] -> Boolean
; consumes an X and a list of X and produces true
; when it is the last element in a list

(check-expect (last? 0 '(0)) #true)
(check-expect (last? 0 '(0 1 2 3)) #false)
(check-expect (last? 3 '(1 2 3)) #true)

(define (last? x lx)
  (equal? x (first (reverse lx))))






























  
