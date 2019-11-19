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
                       (make-node 2 'b NONE NONE)
                       (make-node 3 'c NONE NONE)))

(define BT3
  (make-node 1 'a
             (make-node 2 'b
                        (make-node 9 'c NONE NONE)
                        NONE)
             NONE))

(define BT4
  (make-node 1 'a NONE
             (make-node 2 'a
                        (make-node 3 'a
                                   (make-node 4 'a NONE NONE)
                                   NONE)
                        NONE)))

; Exercise 322

; BT Number -> Boolean
; consumes a Binary Tree bt and a number n and
; produces true if the tree contains n

(check-expect (contains-bt BT1 0) #false)
(check-expect (contains-bt BT1 1) #true)
(check-expect (contains-bt BT2 3) #true)
(check-expect (contains-bt BT3 8) #false)
(check-expect (contains-bt BT3 9) #true)
(check-expect (contains-bt BT4 4) #true)

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
              (contains-bt
               (node-left btree) n))]))

     (define (traverse-right btree)
       (cond
         [(equal? (node-ssn btree) n) #true]
         [(no-info? (node-right btree)) #false]
         [else
          (if (no-info? (node-ssn btree))
              #false
              (contains-bt
               (node-right btree) n))])))

    (cond
      [(no-info? bt) #false]
      [else
       (or (traverse-left bt)
           (traverse-right bt))])))

; Exercise 323

; BT Number -> BT or Boolean
; consumes a BT bt and a number n and produces the
; value at the name field of a node if the ssn is n
; and false otherwise

(check-expect (search-bt BT1 1) 'a)
(check-expect (search-bt BT1 0) #false)
(check-expect (search-bt BT2 2) 'b)
(check-expect (search-bt BT1 4) #false)
(check-expect (search-bt BT3 9) 'c)
(check-expect (search-bt BT4 4) 'a)

(define (fn-search-bt bt n)
  (local
    ((define (fn-search-left btree)
       (cond
         [(no-info? btree) ...]
         [(equal? (node-ssn btree) ...) ...]
         [else
          (fn-search-bt (node-left btree) ...)]))

     (define (fn-search-right btree)
       (cond
         [(no-info? btree) ...]
         [(equal? (node-ssn btree) ...) ...]
         [else
          (fn-search-bt (node-right btree) ...)])))

    (cond
      [(no-info? bt) ...]
      [(equal? (node-ssn bt) n) ...]
      [else
       (... (fn-search-left (node-left bt))
            (fn-search-right (node-right bt)))])))

(define (search-bt bt n)
  (local
    ((define (search-left btree)
       (cond
         [(no-info? btree) #false]
         [(equal? (node-ssn btree) n)
          (node-name btree)]
         [else
          (if (contains-bt (node-left btree) n)
              (search-bt (node-left btree) n)
              (search-bt (node-right btree) n))]))

     (define (search-right btree)
       (cond
         [(no-info? btree) #false]
         [(equal? (node-ssn btree) n)
          (node-name btree)]
         [else
          (if (contains-bt (node-left btree) n)
              (search-bt (node-left btree) n)
              (search-bt (node-right btree) n))])))

    (cond
      [(contains-bt bt n)
       (cond
         [(equal? (node-ssn bt) n)
          (node-name bt)]
         [else
          (if (contains-bt (node-left bt) n)
              (search-left (node-left bt))
              (search-right (node-right bt)))])]
      [else
       #false])))























