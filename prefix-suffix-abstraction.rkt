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

; [ List-of X -> Boolean ] -> [ List-of X ]
; consumes a list and returns the list with
; the last item removed

(check-expect (remove-last '(0)) '())
(check-expect (remove-last '(0 1 2 3)) '(0 1 2))
(check-expect (remove-last '(1 2 3 0)) '(1 2 3))

(define (fn-remove-last lx)
  (local (; [ List-of X ] -> Boolean
          ; ...
          (define (last? x)
            (equal? ... (... (... lx)))))
    (filter last? lx)))

(define (remove-last lx)
  (local (; [ List-of X ] -> Boolean
          ; consumes an item x and produces false
          ; when it is the last element in a list
          (define (last? x)
            (not (equal? x (first (reverse lx))))))
    (filter last? lx)))

; List-of-1Strings -> List-of List-of-1Stirngs
; consumes a list of 1 strings lo1 and produces
; all prefixes

(define (prefix-filter lo1)
  (cond
    [(empty? lo1) '()]
    [else
     (cons lo1
           (prefix-filter (remove-last lo1)))]))

;; suffix

(define (suffix-filter lo1)
  (cond
    [(empty? lo1) '()]
    [else
     (cons lo1
           (suffix-filter
            (remove-first lo1)))]))

(define (remove-first lx)
  (local (; [ List-of X ] -> Boolean
          ; consumes an item x and produces false
          ; when it is the first element in a list
          (define (first? x)
           (not (equal? x (first lx)))))
    (filter first? lx)))

 


























  
