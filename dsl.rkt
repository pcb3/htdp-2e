;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;===============================
;; 22.3 Domain-Specific Languages

(require 2htdp/universe)
(require 2htdp/image)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of three items:
;   (cons FSM-State (cons FSM-State (cons Key '())))
; An FSM-State is a String that specifies a color
; A Key is a String that specifies a key press
 
; data examples 
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

















































