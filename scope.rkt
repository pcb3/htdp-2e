;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname scope) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Scope

(require 2htdp/abstraction)

; design enumerate

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index

(check-expect (enumerate '()) '())
(check-expect (enumerate '(a b c))
              (list (list 'a 0) (list 'b 1) (list 'c 2)))

(define (fn-enumerate lx)
  (local ((define index
            (... (length lx) (lambda (...) ...))))
    (... ... lx ...)))

(define (enumerate lx)
  (local ((define index
            (build-list (length lx)
                        (lambda (i) i))))
    (map list lx index)))


; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2
(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
  (local (; creates a list of each element in l1
          (define create-list (map list l1))

          ; consumes the second list and extracts the
          ; first element of l2
          (define (extract l)
            (cond
              [(empty? (rest l))
               (add-element (first l)
                            create-list)]
              [else
               (append (add-element (first l)
                                  create-list)
                     (extract (rest l)))]))

          ; adds the elements of l1 to l2
          (define (add-element l ll)
            (cond
              [(empty? ll) '()]
              [else
               (cons (cons l (first ll))
                       (add-element l (rest ll)))])))
    (extract l2)))


















