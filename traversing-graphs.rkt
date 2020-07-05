;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname traversing-graphs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 29 Algorithms that Backtrack

;; 29.1 Traversing graphs

;;====
;; 471

(define sample-graph
  (list
   (list 'A 'B 'E)
   (list 'B 'E 'F)
   (list 'C 'D)
   (list 'D)
   (list 'E 'C 'F)
   (list 'F 'D 'G)
   (list 'G)))

; A Node is a Symbol.

; A Graph is one of:
; - '()
; - (cons (list Node) Graph)
; a Graph is a list of Symbols called Nodes that are connected
; by an edge. A Graph can be directed or undirected.

; Node Graph -> List-of Node
; consumes a Node n and a Graph g and produces the list of
; immediate neighbors of n in g.

(check-expect (neighbours 'A sample-graph) '(B E))
(check-expect (neighbours 'C sample-graph) '(D))
(check-expect (neighbours 'D sample-graph) '())

(define (fn-neighbours n g)
  (cond
    [(empty? g) ...]
    [(symbol=? n (first (first g))) ...]
    [else
     (fn-neighbours n (rest g))]))

(define (neighbours n g)
  (cond
    [(empty? g) '()]
    [(symbol=? n (first (first g))) (rest (first g))]
    [else
     (neighbours n (rest g))]))

;;=============

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(define (fn-find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbours origination G))
                  (define candidate
                    (fn-find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (fn-find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (fn-find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (fn-find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

; (map (lambda (x)
;         (fn-find-path
;          x 'G sample-graph))
;       (neighbours 'A sample-graph))

;;==================
;; test-on-all-nodes

; Graph -> Boolean
; consumes a Graph g and produces true if there is a path
; between any pair of nodes

(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes '((A B C)
                                   (B C A)
                                   (C B A))) #true)

(define (test-on-all-nodes g)
  (andmap
   (lambda (x) (= (length g) (length x))) g))

(define cyclic-graph '((A B E)
                       (B E F)
                       (E C F)
                       (C B D)
                       (F D G)
                       (D)
                       (G)))

;;====
;; 473

(check-expect (find-path 'B 'C cyclic-graph) '(B E C))
(check-expect (test-on-all-nodes cyclic-graph) #false)

;;====
;; 474

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

(define (find-path origination destination G)
  (local
    ((define (find-path/list lo-Os D G)
       (cond
         [(empty? lo-Os) #false]
         [else (local ((define candidate
                         (find-path (first lo-Os) D G)))
                 (cond
                   [(boolean? candidate)
                    (find-path/list (rest lo-Os) D G)]
                   [else candidate]))])))
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbours origination G))
                    (define candidate
                      (find-path/list next destination G)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))
 
;;====
;; 475

;;===== to do

;;====
;; 476

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does an-fsm recognize the given string
(check-expect (fsm-match? fsm-a-bc*-d "p") #false)
(check-expect (fsm-match? fsm-a-bc*-d "amd") #false)
(check-expect (fsm-match? fsm-a-bc*-d "abcpd") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "acd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abbbbcccd") #true)

(define (fn-fsm-match? an-fsm a-string)
  (cond
    ;[(empty? a-string) ...]
    [(string=? (fsm-initial an-fsm) (fsm-final an-fsm))
     ...]
    [else
     (local
       ((define 1string (explode a-string))
        (define (next-state lot lo1s)
          (cond
            [(empty? lot) ...]
            [(and (string=? (fsm-initial an-fsm)
                            (transition-current (first lot)))
                  (string=? (transition-key (first lot))
                            (first lo1s)))
             (fn-fsm-match?
              (make-fsm (transition-next (first lot))
                        (fsm-transitions an-fsm)
                        (fsm-final an-fsm))
              (implode (rest lo1s)))]
            [else
             (next-state (rest lot) lo1s)])))
       (next-state (fsm-transitions 1string)))]))

(define (fsm-match? an-fsm a-string)
  (cond
    [(string=? (fsm-initial an-fsm) (fsm-final an-fsm))
     #true]
    [else
     (local
       ((define 1string (explode a-string))
        (define (next-state lot lo1s)
          (cond
            [(empty? lot) #false]
            [(and (string=? (fsm-initial an-fsm)
                            (transition-current (first lot)))
                  (string=? (transition-key (first lot))
                            (first lo1s)))
             (fsm-match?
              (make-fsm (transition-next (first lot))
                        (fsm-transitions an-fsm)
                        (fsm-final an-fsm))
              (implode (rest lo1s)))]
            [else
             (next-state (rest lot) lo1s)])))
       (next-state (fsm-transitions an-fsm) 1string))]))
          
;;====
;; 477

; What is the trivilly solvable problem?

; - '('())

; How are trivial solutions solved?

; - When the list is empty a list of an empty list is produced

; How does the algorithm generate new problems that are more
; easily solvable than the original one? Is there one new
; problem that we generate or are there several?

; - Each recursive call the algorithm produces a list of X
; That is smaller by one letter

; Is the solution of the given problem the same as the
; solution of (one of) the new problems?

; - The  solution is produced by appending the solution of
; the smaller problems together

; Termination: the algorithm will terminate if the list of
; words has finite length
