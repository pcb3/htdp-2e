;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname termination) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 26.2 Termination

; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

(check-expect (bundle (explode "abcdefg") 3)
              '("abc" "def" "g"))

;;====
;; 433

(define ERROR "error! not a valid input")

; [List-of 1String] N -> [List-of String]
; bundles sub-squences of s into stringgs of length n
; termination (bundle s 0) loops unless s '()
(check-expect (checked-bundle '() 0) '())
(check-expect (checked-bundle (explode "abcde") 2)
              '("ab" "cd" "e"))
(check-expect (checked-bundle '("a") 2) '("a"))
(check-error (checked-bundle '("a") 0) ERROR)
(check-error (checked-bundle '(() ()) 1) ERROR)
(check-error (checked-bundle '(1234) 0) ERROR)

(define (fn-checked-bundle s n)
  (local
    ((define (fn-check-s los)
       (cond
         [(list? los) ...]
         [(boolean=? ... (andmap string? los)) ...]
         [(... (> (length los) 0) (= n 0)) ...]
         [else ...])))

    (if (fn-check-s s) (bundle s n) ...)))
              
(define (checked-bundle s n)
  (local
    ((define (check-s los)
       (cond
         [(not (list? los)) #false]
         [(and (not (empty? los)) (= n 0)) #false]
         [(boolean=? #false (andmap string? los)) #false]
         [else #true])))

    (if (check-s s) (bundle s n) (error ERROR))))


;;====
;; 434

; the "else if" clause in smallers that uses '<=' means that
; when its compared to the pivot n then the list may not
; get smaller and so recursive calls never terminate with
; certain inputs

;;====
;; 435

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon))
                  (define list-of-pivots
                    (filter (lambda (x) (= pivot x)) alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    list-of-pivots
                    (quick-sort< (largers (rest alon) pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))





















            