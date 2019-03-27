;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A simulation showing the effects of exponential growth on the length
; of a snake

(require 2htdp/universe)
(require 2htdp/image)

; Physical constants

(define RADIUS 5)
(define SEGMENT-WIDTH (* RADIUS 2))
(define W (* 40 SEGMENT-WIDTH))
(define H (* 40 SEGMENT-WIDTH))

; Graphical constants

(define SEGMENT (circle SEGMENT-WIDTH "solid" "red"))
(define MT (empty-scene W H))
(define MSG (text "GAME OVER" 20 "black"))

(define-struct snake [position length])
; A Snake is a structure:
; (make-snake Posn Number)
; Interpretation: a snake s represents the positon of the snake on the screen
; in number of segments from the top and left border, and
; the length of the snake is measured in segments.

(define SNAKE1 (make-snake (make-posn 40 40) 1))
(define SNAKE2 (make-snake (make-posn 10 10) 10))
(define SNAKE3 (make-snake (make-posn 30 60) 2))

(define-struct tail [position direction])
; A Tail is a structure:
; (make-tail Posn String)
; Interpretation: A (make-tail p s) is a connected segment of the snake
; including the head. p is a Posn with an x and y coordinate.
; s is String that indictates the direction of the segment; "up",
; "down", "left", "right", and "".

(define TAIL1 (make-tail (make-posn 0 0) ""))
(define TAIL2 (make-tail (make-posn 10 100) "up"))
(define TAIL3 (make-tail (make-posn 200 200) "right"))

; A LoCS (list of connected segments) is one of:
; - '()
; - (cons tail LOCS)
; Interpretation: A LoCS is a list of connected (possibly empty)
; segments and their coordinates represented by a Posn.
; A segment is considered connected if only one coordinate
; is different than it's predecessor.

(define LOCS1 (cons (make-tail (make-posn 0 0) "down")
                    (cons (make-tail (make-posn 10 0) "right")
                          (cons (make-tail (make-posn 10 200) "up")
                                (cons (make-tail (make-posn 123 200) "left")
                                      '())))))

; WorldState -> Image
; renders the next image from the current world state

(check-expect (render (make-snake (make-posn 0 0) 1))
              (place-image SEGMENT 0 0 MT))

(check-expect (render (make-snake (make-posn 10 10) 1))
              (place-image SEGMENT 10 10 MT))

(define (render s)
  (place-image SEGMENT
               (posn-x (snake-position s))
               (posn-y (snake-position s))
               MT))

; Snake KeyEvent -> Snake
; listens for a key press and updates the snakes positon based on the key

(check-expect (control (make-snake (make-posn 0 0) 1) "right")
              (make-snake (make-posn 10 0) 1))

(check-expect (control (make-snake (make-posn 100 100) 1) "left")
              (make-snake (make-posn 90 100) 1))

(check-expect (control (make-snake (make-posn 50 50) 1) "up")
              (make-snake (make-posn 50 40) 1))

(check-expect (control (make-snake (make-posn 80 80) 1) "down")
              (make-snake (make-posn 80 90) 1))

(check-expect (control (make-snake (make-posn 0 0) 1) "x")
              (make-snake (make-posn 0 0) 1))

(define (control s key)
  (cond
    [(string=? "left" key)
     (make-snake (make-posn (- (posn-x (snake-position s)) SEGMENT-WIDTH)
                            (posn-y (snake-position s)))
                 (snake-length s))]
    [(string=? "right" key)
     (make-snake (make-posn (+ SEGMENT-WIDTH (posn-x (snake-position s)))
                            (posn-y (snake-position s)))
                 (snake-length s))]
    [(string=? "up" key)
     (make-snake (make-posn (posn-x (snake-position s))
                            (- (posn-y (snake-position s)) SEGMENT-WIDTH))
                 (snake-length s))]
    [(string=? "down" key)
     (make-snake (make-posn (posn-x (snake-position s))
                            (+ SEGMENT-WIDTH (posn-y (snake-position s))))
                 (snake-length s))]
    [else s]))                 

; Snake -> Snake
; with each new state the snake moves one segment-width

(check-expect (tock (make-snake (make-posn 10 10) 1))
              (make-snake (make-posn 20 20) 1))

(define (tock s)
  (make-snake (make-posn (+ SEGMENT-WIDTH (posn-x (snake-position s)))
                         (+ SEGMENT-WIDTH (posn-y (snake-position s))))
              (snake-length s)))

; Snake -> Boolean
; consumes a snake s and outputs true if the head of the  snake hits the
; screen boundary

(check-expect (last-world? (make-snake (make-posn 200 200) 1)) #false)

(check-expect (last-world? (make-snake (make-posn 395 200) 1)) #true)

(check-expect (last-world? (make-snake (make-posn 200 395) 1)) #true)

(check-expect (last-world? (make-snake (make-posn 5 200) 1)) #true)

(check-expect (last-world? (make-snake (make-posn 200 5) 1)) #true)

(define (last-world? s)
  (cond
    [(or (<= (posn-x (snake-position s)) RADIUS)
         (>= (posn-x (snake-position s)) (- W RADIUS))
         (<= (posn-y (snake-position s)) RADIUS)
         (>= (posn-y (snake-position s)) (- H RADIUS)))
     #true]
    [else
     #false]))

; Snake -> Image
; if last-world is true renders the last scene of the world

(define (last-picture s)
  (place-image MSG
               (/ W 2)
               (/ H 2)
               (render s)))

; Snake -> Snake
; launches the program from some initial state ws

(define (snake-main rate)
   (big-bang (make-snake (make-posn 40 40) 1)
     [on-tick tock rate]
     [to-draw render]
     [on-key control]
     [stop-when last-world? last-picture]))

; --usage
;(snake-main 1)






