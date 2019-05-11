;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; htdp 2e: part II FSM

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
