;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; htdp 2e: part II FSM

(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; FSM-State  FSM-State -> Boolean
; consumes two states s1 s2, and returns true if they are equal

(check-expect (state=? 1 1) #true)

(check-expect (state=? "hello" "hello") #true)

(check-expect (state=? "harry" "ron") #false)

(define (fn-state=? s1 s2)
  (cond
    [(equal? s1 s2) ...]
    [else ...]))

(define (state=? s1 s2)
  (cond
    [(equal? s1 s2) #true]
    [else #false]))

; A BW machine is a:
; - Transition

; Interpretation A BW represents the sequence of transitions from black to white
; and back after a key press

(define BW (list (make-transition "black" "white")
                 (make-transition "white" "black")))

; A SimulationState.v1 is an FSM-State. 

;(define (simulate.v1 fsm0)
;  (big-bang initial-state
;    [to-draw render-state.v1]
;    [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a world state as an image 
(define (render-state.v1 s)
  empty-image)
 
; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
  cs)

;; v2

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)

; SimulationState.v2 -> Image
; renders a world state as an image 
(define (render-state.v2 s)
  empty-image)
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm))))

(check-expect
 (find-next-state (make-fs fsm-traffic "red") "n")
 (make-fs fsm-traffic "green"))

(check-expect
 (find-next-state (make-fs fsm-traffic "red") "a")
 (make-fs fsm-traffic "green"))

(check-expect
 (find-next-state (make-fs fsm-traffic "green") "q")
 (make-fs fsm-traffic "yellow"))

(check-expect
 (find-next-state (make-fs fsm-traffic "yellow") "z")
 (make-fs fsm-traffic "red"))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
               (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

(check-expect (find fsm-traffic "red") "green")

(check-expect (find fsm-traffic "green") "yellow")

(check-expect (find fsm-traffic "yellow") "red")

(check-error (find fsm-traffic "black")
             "not found: black")

(define (fn-find transitions current)
  (cond
    [(empty? transitions) ...]
    [else (if (... (transition-current (first transitions))
                   current)
              (transition-next (first transitions))
              (fn-find (rest transitions) current))]))

(define (find transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (if (state=? (transition-current (first transitions))
                       current)
              (transition-next (first transitions))
              (find (rest transitions) current))]))





















