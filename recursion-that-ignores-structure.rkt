;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname recursion-that-ignores-structure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 25.2 Recursion that ignores structure

;;====
;; 424

;;===========
;; quick-sort

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; [List-of Number] Number -> [List-of Number]
; consumes a list of numbers alon and produces a list that is
; strictly smaller than the pivot n
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
; consumes a list of numbers alon and produces a list that is
; strictly larger than the pivot n
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

;;====
;; 427

; [List-of Number] -> [List-of Number]
; consumes a list of number alon and sorts them in decreasing
; order
(check-expect (quick-sort> '()) '())
(check-expect (quick-sort> '(1)) '(1))
(check-expect (quick-sort> '(2 1 3)) '(3 2 1))
(check-expect (quick-sort> '(1 2 4 3 6 5 8 7 9 11 10))
              '(11 10 9 8 7 6 5 4 3 2 1))
(check-expect (quick-sort> '(1 1 1 1 1 1 1 1 1 1 1))
              '(1 1 1 1 1 1 1 1 1 1 1))

(define (fn-quick-sort> alon)
  (cond
    [(empty? alon) ...]
    [(= (length alon) ...) ...]
    [(< (length alon) ...) (fn-liam-sort> alon)]
    [else
     (local
       ((define pivot (first alon))

        (define (fn-largers> aln pvt)
          (cond
            [(empty? aln) ...]
            [else (if (> (... aln) pvt)
                      (... (first aln)
                           (fn-largers> (rest aln) pvt))
                      (fn-largers> (rest aln) pvt))]))

        (define (fn-smallers> aln pvt)
          (cond
            [(empty? aln) ...]
            [else (if (< (... aln) pvt)
                      (... (first aln)
                           (fn-smallers> (rest aln) pvt))
                      (fn-smallers> (rest aln) pvt))])))
        
       (append (fn-quick-sort> (fn-largers> alon pivot))
               (list pivot)
               (fn-quick-sort> (fn-smallers> alon pivot))))]))

(define (fn-liam-sort> l)
  (cond
    [(empty? l) ...]
    [(cons? l)
     (fn-insert (... l) (fn-liam-sort> (rest l)))]))
        
(define (fn-insert n l)
  (cond
    [(empty? l) (... n '())]
    [else (if (... n (... l))
              (... n l)
              (... (first l) (fn-insert n (rest l))))]))

(define (quick-sort> alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [(<= (length alon) 10) (liam-sort> alon)]
    [else
     (local
       ((define  pivot (first alon))
        (define list-of-pivots
          (filter (lambda (x) (= pivot x)) alon)))
       (append (quick-sort> (largers> alon pivot))
               list-of-pivots
               (quick-sort>
                (reverse (smallers< alon pivot)))))]))

(define (largers> aln pvt)
  (cond
    [(empty? aln) '()]
    [else (if (> (first aln) pvt)
              (cons (first aln) (largers> (rest aln) pvt))
              (largers> (rest aln) pvt))]))

(define (smallers< aln pvt)
  (cond
    [(empty? aln) '()]
    [else
     (if (< (first aln) pvt)
         (cons (first aln) (smallers< (rest aln) pvt))
         (smallers< (rest aln) pvt))]))

(define (liam-sort> l)
  (cond
    [(empty? l) '()]
    [else (insert (first l) (liam-sort> (rest l)))]))

(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (> n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

;;====
;; 429

; [List-of Number] Number -> [List-of Number]
; consumes a list of numbers alon and a number n and produces
; a list larger than n
(check-expect (larger> '(1 2 3) 1) '(2 3))
(check-expect (larger> '(1 2 3) 4) '())
(check-expect (larger> '() 0) '())

(define (larger> alon n)
  (filter (lambda (y) (> y n)) alon))

; [List-of Number] Number -> [List-of Number]
; consumes a list of numbers alon and a number n and produces
; a list smaller than n
(check-expect (smaller< '(1 2 3) 0) '())
(check-expect (smaller< '(1 2 3) 4) '(1 2 3))
(check-expect (smaller< '() 0) '())

(define (smaller< alon n)
  (filter (lambda (x) (< x n)) alon))

;;====
;; 430

; [List-of Number] -> [List-of Number]
; consumes a list of number alon and sorts them in decreasing
; order
(check-expect (my-quick-sort< '()) '())
(check-expect (my-quick-sort< '(1)) '(1))
(check-expect (my-quick-sort< '(3 2 1)) '(1 2 3))
(check-expect (my-quick-sort<
               (reverse '(1 2 4 3 6 5 8 7 9 11 10)))
              '(1 2 3 4 5 6 7 8 9 10 11))
(check-expect (my-quick-sort< '(1 1 1 1 1 1 1 1 1 1 1))
              '(1 1 1 1 1 1 1 1 1 1 1))

(define (my-quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    
    [else
     (local
       (
        (define  pivot (first alon))

        (define list-of-pivots
          (filter (lambda (x) (= pivot x)) alon))

        (define (small< l p)
          (cond
            [(empty? l) '()]
            [else (if (< (first l) p)
                      (cons (first l) (small< (rest l) p))
                      (small< (rest l) p))]))

        (define (large< l p)
          (cond
            [(empty? l) '()]
            [(or (< (first l) p) (= (first l) p))
             (large< (rest l) p)]
            [else (cons (first l) (large< (rest l) p))]))

        (define (sort< l)
          (cond
            [(empty? l) '()]
            [else (insert< (first l) (sort< (rest l)))]))

        (define (insert< n l)
          (cond
            [(empty? l) (cons n '())]
            [else (if (< n (first l))
                      (cons n l)
                      (cons (first l) (insert< n (rest l))))])))

       (if (<= (length alon) 10)
           (sort< alon)
           (append (my-quick-sort< (small< alon pivot))
                   list-of-pivots
                   (my-quick-sort<
                    (reverse (large< alon pivot))))))]))






































