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

(define LOCS1 '())

(define LOCS2 (cons (make-tail (make-posn 200 200) "")
                    '()))

(define LOCS3 (cons (make-tail (make-posn 0 0) "down")
                    (cons (make-tail (make-posn 0 20) "right")
                          (cons (make-tail (make-posn 20 20) "down")
                                (cons (make-tail (make-posn  20 40) "")
                                      '())))))

(define LOCS4 (cons (make-tail (make-posn 200 200) "")
                    (cons (make-tail (make-posn 220 200) "")
                          (cons (make-tail (make-posn 220 220) "")
                                (cons (make-tail (make-posn 240 220) "")
                                      (cons (make-tail (make-posn 260 220) "")
                                '()))))))

; LoCS -> Image
; consumes a list of connected segments (locs) and draws each segment
; to the screen.

(check-expect (render-connected '()) MT)

(check-expect (render-connected
               (cons (make-tail (make-posn 200 200) "")
                     '()))
              (place-images
               (list SEGMENT)
               (list (make-posn 200 200))
               MT))

(check-expect (render-connected
               (cons (make-tail (make-posn 200 200) "down")
                     (cons (make-tail (make-posn 200 220) "down")
                           (cons (make-tail (make-posn 200 240) "right")
                                 (cons (make-tail (make-posn 220 240) "up")
                                       '())))))
              (place-images
               (list SEGMENT SEGMENT SEGMENT SEGMENT)
               (list (make-posn 200 200)
                     (make-posn 200 220)
                     (make-posn 200 240)
                     (make-posn 220 240))
               MT))

(define (fn-render-connected locs)
  (cond
    [(empty? locs) ...]
    [else (... SEGMENT
               (posn-x (tail-position (first locs)))
               (posn-y (tail-position (first locs)))
               (fn-render-connected (rest locs)))]))
               
(define (render-connected locs)
  (cond
    [(empty? locs) MT]
    [else (place-image SEGMENT
                       (posn-x (tail-position (first locs)))
                       (posn-y (tail-position (first locs)))
                       (render-connected (rest locs)))]))

; Snake -> Image
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

; LoCS KeyEvent -> LoCS
; listens for a key press and updates the snakes head direction accordingly

(check-expect (control-connected '() "") '())

(check-expect (control-connected
               (cons (make-tail (make-posn 200 200) "")
                     '()) "")
              (cons (make-tail (make-posn 200 200) "")
                    '()))

(check-expect (control-connected
               (cons (make-tail (make-posn 200 200) "")
                     '()) "right")
              (cons (make-tail (make-posn 200 200) "right")
                    '()))

(check-expect (control-connected
               (cons (make-tail (make-posn 200 200) "right")
                     '()) "up")
              (cons (make-tail (make-posn 200 200) "up")
                    '()))

(check-expect (control-connected
               (cons (make-tail (make-posn 200 200) "left")
                     (cons (make-tail (make-posn 180 200) "down")
                           '())) "right")
              (cons (make-tail (make-posn 200 200) "left")
                    (cons (make-tail (make-posn 180 200) "right")
                          '())))

(define (fn-control-connected locs key)
  (cond
    [(empty? locs) ...]
    [(string=? "left" key)
     (... locs key)]
    [(string=? "right" key)
     (... locs key)]
    [(string=? "up" key)
     (... locs key)]
    [(string=? "down" key)
     (... locs key)]
    [else locs]))

(define (control-connected locs key)
  (cond
    [(empty? locs) locs]
    [(string=? "left" key)
     (recur-replace-head locs key)]
    [(string=? "right" key)
     (recur-replace-head locs key)]
    [(string=? "up" key)
     (recur-replace-head locs key)]
    [(string=? "down" key)
     (recur-replace-head locs key)]
    [else locs]))

; LoCS key -> LoCS
; recurs to the head of the list of connected segments and replaces it with
; a new tail structure with direction equal to key.

(check-expect (recur-replace-head
               (cons (make-tail (make-posn 200 200) "right") '()) "down")
              (cons (make-tail (make-posn 200 200) "down") '()))

(check-expect (recur-replace-head
               (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 220 200) "down")
                           '())) "left")
              (cons (make-tail (make-posn 200 200) "right")
                    (cons (make-tail (make-posn 220 200) "left")
                          '())))

(define (fn-recur-replace-head locs key)
  (cond
    [(empty? (rest locs)) ...]
    [else (... (first locs)
               (fn-recur-place-head (rest locs) key))]))

(define (recur-replace-head locs key)
  (cond
    [(empty? (rest locs))
     (cons (make-tail (make-posn (posn-x (tail-position (first locs)))
                                 (posn-y (tail-position (first locs))))
                      key) '())]
    [else (cons (first locs)
                (recur-replace-head (rest locs) key))]))

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

; LoCS -> LoCS
; with each clock tick the list of connected segments is updated depending
; on the direction field

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "") '()))
              (cons (make-tail (make-posn 200 200) "") '()))

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "right") '()))
              (cons (make-tail (make-posn 220 200) "right") '()))

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 220 200) "down")
                           (cons (make-tail (make-posn 220 220) "left") '()))))
              (cons (make-tail (make-posn 220 200) "down")
                    (cons (make-tail (make-posn 220 220) "left")
                          (cons (make-tail (make-posn 200 220) "left") '()))))

(define (fn-tock-connected locs)
  (cond
    [(empty? (rest locs)) ...]
    [else (... (first (rest locs))
               (fn-tock-connected (rest locs)))]))

(define (tock-connected locs)
  (cond
    [(string=? "" (tail-direction (first (reverse locs))))
     locs]
    [(empty? (rest locs))
     (create-head (first locs))]
    [else (cons (first (rest locs))
                (tock-connected (rest locs)))]))

; Tail -> LoCS
; consumes the tail (head) of the snake. Creates a new list and head
; displaced by twice the diameter of a segment in the direction of the  
; field direction.

(check-expect (create-head
               (make-tail (make-posn 200 200) "right"))
              (cons (make-tail (make-posn 220 200) "right") '()))

(define (fn-create-head head) head)

(define (create-head head)
  (cond
    [(string=? "left" (tail-direction head))
     (cons
      (make-tail (make-posn (- (posn-x (tail-position head)) (* SEGMENT-WIDTH 2))
                            (posn-y (tail-position head)))
                 (tail-direction head)) '())]
    [(string=? "right" (tail-direction head))
     (cons
      (make-tail (make-posn (+ (* SEGMENT-WIDTH 2) (posn-x (tail-position head)))
                            (posn-y (tail-position head)))
                 (tail-direction head)) '())]
    [(string=? "up" (tail-direction head))
     (cons
      (make-tail (make-posn (posn-x (tail-position head))
                            (- (posn-y (tail-position head)) (* SEGMENT-WIDTH 2)))
                 (tail-direction head)) '())]
    [(string=? "down" (tail-direction head))
     (cons
      (make-tail (make-posn (posn-x (tail-position head))
                            (+ (* SEGMENT-WIDTH 2) (posn-y (tail-position head))))
                 (tail-direction head)) '())]
    [else (cons head '())]))

; Snake -> Snake
; with each new state the snake moves one segment-width

(check-expect (tock (make-snake (make-posn 10 10) 1))
              (make-snake (make-posn 20 20) 1))

(define (tock s)
  (make-snake (make-posn (+ SEGMENT-WIDTH (posn-x (snake-position s)))
                         (+ SEGMENT-WIDTH (posn-y (snake-position s))))
              (snake-length s)))

; LoCS -> Boolean
; consumes a list of connected segements and outputs true if the head of the
; snake collides with boundary or itself

(check-expect (last-world-connected?
               (cons (make-tail (make-posn 20 20) "right") '())) #false)

(check-expect (last-world-connected?
               (cons (make-tail (make-posn W 200) "right") '())) #true)

(check-expect (last-world-connected?
               (cons (make-tail (make-posn 200 H) "down") '())) #true)

(check-expect (last-world-connected?
               (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 200 200) "left")
                           '()))) #true)

(define (fn-last-world-connected? locs)
  (cond
    [(or (... (posn-x (tail-position (first (... locs)))) ...)
         (... (posn-x (tail-position (first (... locs)))) ...)
         (... (posn-y (tail-position (first (... locs)))) ...)
         (... (posn-y (tail-position (first (... locs)))) ...))
     ...]
    [else
     ...]))

(define (last-world-connected? locs)
  (cond
    [(or (< (posn-x (tail-position (first (reverse locs)))) RADIUS)
         (> (posn-x (tail-position (first (reverse locs)))) (- W RADIUS))
         (< (posn-y (tail-position (first (reverse locs)))) RADIUS)
         (> (posn-y (tail-position (first (reverse locs)))) (- H RADIUS))
         (head-collide? locs))
     #true]
    [else
     #false]))

; LoCS -> Boolean
; consumes a list of connected segments and outputs true if the coordinates
; of the head is equal to the coordinates of another segment.

(check-expect (head-collide? (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 200 200) "left")
                           '()))) #true)

(check-expect (head-collide? (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 180 180) "right")
                           '()))) #false)

(define (fn-head-collide? locs)
  (cond
    [(empty? (rest locs)) ...]
    [(equal? (... (first (reverse locs)))
             (... (first locs)))
     ...]
    [else (fn-head-collide? (rest locs))]))

(define (head-collide? locs)
  (cond
    [(empty? (rest locs)) #false]
    [(equal? (tail-position (first (reverse locs)))
             (tail-position (first locs)))
     #true]
    [else (head-collide? (rest locs))]))

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

; LoCS -> Image
; if last-world-connected? returns true the last world is displayed

(check-expect (last-picture-connected
               (cons (make-tail (make-posn 200 200) "right") '()))
              (place-image MSG (/ W 2) (/ H 2)
                           (render-connected
                            (cons (make-tail (make-posn 200 200) "right") '()))))
                           
(define (fn-last-picture-connected locs)
  (place-image ... ... ... ...))

(define (last-picture-connected locs)
  (place-image MSG (/ W 2) (/ H 2)
               (render-connected locs)))

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
  (big-bang LOCS4
    [on-tick tock-connected rate]
    [to-draw render-connected]
    [on-key control-connected]
    [stop-when last-world-connected? last-picture-connected]))

; --usage
;(snake-main 1)






