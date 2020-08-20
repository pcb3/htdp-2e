;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname problem-structural-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 31 Accumulators

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
 
(define (add-to-each n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ (first l) n) (add-to-each n (rest l)))]))

;;====
;; 489

(check-expect (cons 50 (add-to-each-map 50 '(40 110 140 170)))
              '(50 90 160 190 220))

(define (add-to-each-map n l)
  (map (lambda (b) (+ n b)) l))

;;====
;; 491

(define (relative->absolute-foldr l)
  (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

; List -> List
; consumes a list l and produces a reversed list

(check-expect (rl '()) '())
(check-expect (rl '(1)) '(1))
(check-expect (rl '(1 2 3)) '(3 2 1))

(define (rl l)
  (foldl (lambda (x y) (cons x y)) '() l))










