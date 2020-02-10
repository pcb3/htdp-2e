;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;===============================
;; 22.3 Domain-Specific Languages

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/abstraction)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons Key '())))
; An FSM-State is a String that specifies a color
; A Key is a String that specifies a key press
 
; data examples
(define fsm-traffic0
  '(("red" "green")
    ("green" "yellow") ("yellow" "red")))

(define fsm-traffic
  '(("red" "green" "right")
    ("green" "yellow" " ") ("yellow" "red" " ")))
 
; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay
        (text current '24 'black)
        (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (cond
         [(and (key=? key-event "right")
               (string=? current "red"))
          (find transitions current)]
         [(and (key=? key-event " ")
               (or (string=? current "green")
                   (string=? current "yellow")))
          (find transitions current)]
         [else current]))]))
 
; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") "green")
; (check-expect (find fsm-traffic "pink") (error "not found"))

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

;;============================

; An XMachine is a nested list of this shape:
;   `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

;;=================
;; 381

; An X1T is a nested list of this shape (using list):
(define X1T0
  (list 'action (list (list 'state "red")
                      (list 'next "green"))))

; An X1T is a nested list of this shape (using cons):
(define X1T1
  (cons 'action
        (cons
         (cons (cons 'state (cons "red" '()))
               (cons (cons 'next (cons "green" '())) '()))
         '())))

; An XMachine is a nested list of thie shape (using list):
(define XMACHINE0
  (list 'machine (list (list 'initial "blue"))
        (list X1T0)))

; An XMachine is a nested list of thie shape (using cons):
(define XMACHINE1
  (cons 'machine
        (cons
         (cons (cons 'initial (cons "blue" '())) '())
         (cons (cons X1T1 '()) '()))))

;;===================
;; 382

; <bwmachine initial="black">
;  <key state="black" next="white" />
;  <key state"white" next="black" />
; </bwmachine>

(define bw0
  '(machine ((initial "black"))
            (action ((state "black") (next "white")))
            (action ((state "white") (next "black")))))

;;=======================
;; Xexpr helper functions

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x '()))])))

; Xexpr.v2 -> List-of Xexpr.v2
; consumes an Xexpr.v2 xe and produces a list of content
; elements
(define (xexpr-content x)
  (local ((define optional-loa+content (rest x)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x
                      (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))

(define (find-attr loa s)
  (cond
    [(empty? loa) #false]
    [else
     (if (list? (assq s loa))
         (second (assq s  loa))
         #false)]))

;;=======================

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

;;=======================
;; 383

; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))
 
; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
 
(check-expect (xm-state0 xm0) "red")
 
(define (xm-state0 xm)
  (find-attr (xexpr-attr xm) 'initial))
 
; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
 
(check-expect (xm->transitions xm0) fsm-traffic0)
 
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

;;======================
;; simulate using bw0

; FSM FSM-State -> FSM-State 
; matches the keys pressed by a player with the given FSM 
(define (simulate1 state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (square 100 "solid" current))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))

(define (simulate-xmachine1 xm)
  (simulate1 (xm-state0 xm) (xm->transitions xm)))

(simulate-xmachine1 bw0)







































