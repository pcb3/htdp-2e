;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A snake clone

(require 2htdp/universe)

; WorldState: data that represents the state of the world (cw)
 
; WorldState -> Image
; when needed, big-bang obtains the image of the current 
; state of the world by evaluating (render cw) 
(define (render ws) ...)
 
; WorldState -> WorldState
; for each tick of the clock, big-bang obtains the next 
; state of the world from (clock-tick-handler cw) 
(define (clock-tick-handler cw) ...)
 
; WorldState String -> WorldState 
; for each keystroke, big-bang obtains the next state 
; from (keystroke-handler cw ke); ke represents the key
(define (keystroke-handler cw ke) ...)

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]))