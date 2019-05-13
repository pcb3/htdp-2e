;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; htdp 2e: part II FSM

(require 2htdp/universe)
(require 2htdp/image)

; An FSM is one of:
;   – '()
;   – (cons Ktransition FSM)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

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
  (make-fsm
   (find ke (fsm-transitions an-fsm) (fsm-initial an-fsm))
   (fsm-transitions an-fsm)
   (fsm-final an-fsm)))

;(check-expect
; (find-next-state (make-fs fsm-traffic "red") "n")
; (make-fs fsm-traffic "red"))
;
;(check-expect
; (find-next-state (make-fs fsm-traffic "white") "a")
; (make-fs fsm-traffic "yellow"))
;
;(check-expect
; (find-next-state (make-fs fsm-traffic "yellow") "b")
; (make-fs fsm-traffic "yellow"))
;
;(check-expect
; (find-next-state (make-fs fsm-traffic "yellow") "c")
; (make-fs fsm-traffic "yellow"))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
;(check-expect (state-as-colored-square
;               (make-fs fsm-traffic "red"))
;              (square 100 "solid" "red"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field

;(check-expect (find fsm-traffic "red") "green")
;
;(check-expect (find fsm-traffic "green") "yellow")
;
;(check-expect (find fsm-traffic "yellow") "red")
;
;(check-error (find fsm-traffic "black")
;             "not found: black")

(define (fn-find ke transitions current)
  (cond
    [(empty? transitions) ...]
    [else (if (... (... (ktransition-current (first transitions)) current)
                   (... (ktransitions-key (first transitions)) ke))
              (ktransition-next (first transitions))
              (fn-find ke (rest transitions) current))]))

(define (find ke transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (if (and (state=? (ktransition-current (first transitions)) current)
                   (equal? (ktransition-key (first transitions)) ke))
              (ktransition-next (first transitions))
              (find ke (rest transitions) current))]))

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

(define fsm-sequence (list (make-ktransition "white" "a" "yellow")
                           (make-ktransition "yellow" "b" "yellow")
                           (make-ktransition "yellow" "c" "yellow")
                           (make-ktransition "yellow" "d" "green")))

;; FSM

(define-struct fsm [initial transitions final])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (fsm-simulate initial an-fsm final)
  (big-bang (make-fsm initial an-fsm final)
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when final? final-pic]))

; SimulationState.v2 -> SimulationState.v2
; consumes a simulation state and stops the program when the it's in the
; final state
(define (final? fsm)
  (cond
    [(equal? (fsm-initial fsm) (fsm-final fsm))
     #true]
    [else
     #false]))

(define (final-pic fsm)
  (square 100 "solid" (fsm-initial fsm)))
  
	






 




















