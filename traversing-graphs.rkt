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

(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbours origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

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
















