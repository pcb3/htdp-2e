;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname BSTs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 19.5 Project: BSTs

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BT (BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

(define BT1 (make-node 1 'a NONE NONE))

(define BT2 (make-node 1 'a
                       (make-node 2 'a NONE NONE)
                       (make-node 3 'a NONE NONE)))

(define BT3
  (make-node 1 'a
             (make-node 2 'a
                        (make-node 9 'a NONE NONE)
                        NONE)
             NONE))

; Exercise 322

; BT Number -> Boolean
; consumes a Binary Tree bt and a number n and
; produces true if the tree contains n

(check-expect (contains-bt BT1 0) #false)
(check-expect (contains-bt BT1 1) #true)
(check-expect (contains-bt BT2 3) #true)
(check-expect (contains-bt BT3 8) #false)
(check-expect (contains-bt BT3 9) #true)

(define (fn-contains-bt bt n)
  (local
    ((define (fn-traverse-left btree)
       (cond
         [(equal? (node-ssn btree) n) ...]
         [else
          (... (no-info? (node-ssn btree))
               ...
               (fn-traverse-left
                (node-left btree)))]))

     (define (fn-traverse-right btree)
       (cond
         [(equal? (node-ssn btree) n) ...]
         [else
          (... (no-info? (node-ssn btree))
               ...
               (fn-traverse-right
                (node-right btree)))])))

    (... (fn-traverse-left bt)
         (fn-traverse-right bt))))
    
(define (contains-bt bt n)
  (local
    ((define (traverse-left btree)
       (cond
         [(equal? (node-ssn btree) n) #true]
         [(no-info? (node-left btree)) #false]
         [else
          (if (no-info? (node-ssn btree))
               #false
               (traverse-left
                (node-left btree)))]))

     (define (traverse-right btree)
       (cond
         [(equal? (node-ssn btree) n) #true]
         [(no-info? (node-right btree)) #false]
         [else
          (if (no-info? (node-ssn btree))
               #false
               (traverse-right
                (node-right btree)))])))

    (or (traverse-left bt)
         (traverse-right bt))))












