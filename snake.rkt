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
(define MAX (- W SEGMENT-WIDTH))

; Graphical constants

(define SEGMENT (circle SEGMENT-WIDTH "solid" "red"))
(define FOOD (circle SEGMENT-WIDTH "solid" "orange"))
(define MT (empty-scene W H))
(define MSG (text "GAME OVER" 20 "black"))

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

(define-struct snake [locs position])
; A Snake is a structure:
; (make-snake LoCS Posn)
; Interpretation: A (make-snake l p) is a representation of the programs state.
; A Locs is a list of connected segments and Position is the coordinates of
; the edibles.

(define SNAKE1 (make-snake
                (cons (make-tail (make-posn 200 200) "") '())
                (make-posn 100 100)))

(define SNAKE2 (make-snake
                (cons (make-tail (make-posn 200 200) "")
                      (cons (make-tail (make-posn 220 200) "right")
                            '()))
                (make-posn 240 200)))

(define SNAKE3 (make-snake
                (cons (make-tail (make-posn 200 200) "right")
                      (cons (make-tail (make-posn 220 200) "right")
                            (cons (make-tail (make-posn 240 200) "right")
                                  '())))
                (make-posn 100 100)))
                                       

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
; Snake -> Image
; consumes a snake and renders the state of the world to the screen
;(define (render 


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
    [else
     (place-image SEGMENT
                  (posn-x (tail-position (first locs)))
                  (posn-y (tail-position (first locs)))
                  (render-connected (rest locs)))]))

; Snake -> Image
; consumes a Snake s and renders an image of Food

(check-expect (render-edible
               (make-snake (cons (make-tail (make-posn 200 200) "") '())
                           (make-posn 350 350)) MT)
              (place-image FOOD 350 350 MT))

(define (fn-render-edible s scene)
  (cond
    [(edible-collide? s)
     (...
      ...
      (posn-x (... (snake-position s)))
      (posn-y (... (snake-position s)))
      scene)]
    [else 
     (...
      (posn-x (snake-position s))
      (posn-y (snake-position s))
      scene)]))

(define (render-edible s scene)
  (place-image FOOD
               (posn-x (snake-position s))
               (posn-y (snake-position s))
               scene))

; LoCS Snake -> Image
; renders the world given a snake s

(define (fn-render-world s)
  (... s (... (snake-locs s))))

(define (render-world s)
  (render-edible s 
                 (render-connected (snake-locs s))))

; Snake KeyEvent -> Snake
; listens for a key press and updates the snakes head direction accordingly

(check-expect (control-connected
               (make-snake
                (cons (make-tail (make-posn 200 200) "right")
                      (cons (make-tail (make-posn 220 200) "right")
                            '()))
                (make-posn 100 100))
               "down")
              (make-snake
               (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 220 200) "down")
                           '()))
               (make-posn 100 100)))
                

(define (fn-control-connected s key)
  (cond
    [(empty? (snake-locs s)) ...]
    [(string=? "left" key)
     (... (snake-locs s) key)]
    [(string=? "right" key)
     (... (snake-locs s) key)]
    [(string=? "up" key)
     (... (snake-locs s) key)]
    [(string=? "down" key)
     (... (snake-locs s) key)]
    [else (snake-locs s)]))

(define (control-connected s key)
  (cond
    [(empty? (snake-locs s))
     s]
    [(string=? "left" key)
     (make-snake
      (recur-replace-head (snake-locs s) key) (snake-position s))]
    [(string=? "right" key)
     (make-snake (recur-replace-head (snake-locs s) key) (snake-position s))]
    [(string=? "up" key)
     (make-snake (recur-replace-head (snake-locs s) key) (snake-position s))]
    [(string=? "down" key)
     (make-snake (recur-replace-head (snake-locs s) key) (snake-position s))]
    [else s]))

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

; Snake -> Snake
; consumes a snake returns an updated list of connected segments after each tick

(check-expect (tock
               (make-snake (cons (make-tail (make-posn 200 200) "up") '())
                           (make-posn 100 100)))
              (make-snake (cons (make-tail (make-posn 200 180) "up") '())
                          (make-posn 100 100)))

(define (fn-tock s)
  (make-snake (... (snake-locs s) s)
              (snake-position s)))

(define (tock s)
  (make-snake (tock-connected (snake-locs s) s)
              (snake-position s)))

; Snake LoCS -> LoCS
; with each clock tick the list of connected segments is updated depending
; on the direction field and position of edible

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "") '())
               (make-snake
                (cons (make-tail (make-posn 200 200) "") '())
                (make-posn 100 100)))
              (cons (make-tail (make-posn 200 200) "") '()))

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "right") '())
               (make-snake
                (cons (make-tail (make-posn 200 200) "right") '())
                (make-posn 100 100)))
              (cons (make-tail (make-posn 220 200) "right") '()))

(check-expect (tock-connected
               (cons (make-tail (make-posn 200 200) "right")
                     (cons (make-tail (make-posn 220 200) "down")
                           (cons (make-tail (make-posn 220 220) "left") '())))
               (make-snake
                (cons (make-tail (make-posn 200 200) "right")
                      (cons (make-tail (make-posn 220 200) "down")
                            (cons (make-tail (make-posn 220 220) "left") '())))
                (make-posn 100 100)))
              (cons (make-tail (make-posn 220 200) "down")
                    (cons (make-tail (make-posn 220 220) "left")
                          (cons (make-tail (make-posn 200 220) "left") '()))))

(define (fn-tock-connected locs s)
  (cond
    [(empty? (rest locs)) ...]
    [else (... (first (rest locs))
               (fn-tock-connected (rest locs) s))]))

(define (tock-connected locs s)
  (cond
    [(string=? "" (tail-direction (first (reverse locs))))
     locs]
    [(edible-collide? s)
     (grow s)]
    [(empty? (rest locs))
     (create-head (first locs))]
    [else (cons (first (rest locs))
                (tock-connected (rest locs) s))]))

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

; Snake -> Boolean
; consumes a snake and outputs true if the head of the
; snake collides with boundary or itself

(check-expect (last-world-connected?
               (make-snake
                (cons (make-tail (make-posn 20 20) "right") '())
                (make-posn 100 100))) #false)

(check-expect (last-world-connected?
               (make-snake
                (cons (make-tail (make-posn W 200) "right") '())
                (make-posn 100 100))) #true)

(check-expect (last-world-connected?
               (make-snake
                (cons (make-tail (make-posn 200 H) "down") '())
                (make-posn 100 100))) #true)

(check-expect (last-world-connected?
               (make-snake
                (cons (make-tail (make-posn 200 200) "right")
                      (cons (make-tail (make-posn 200 200) "left")
                            '())) (make-posn 100 100))) #true)

(define (fn-last-world-connected? s)
  (cond
    [(or (... (posn-x (tail-position (first (... s)))) ...)
         (... (posn-x (tail-position (first (... s)))) ...)
         (... (posn-y (tail-position (first (... s)))) ...)
         (... (posn-y (tail-position (first (... s)))) ...))
     ...]
    [else
     ...]))

(define (last-world-connected? s)
  (cond
    [(or (< (posn-x (tail-position (first (reverse (snake-locs s))))) RADIUS)
         (> (posn-x (tail-position (first (reverse (snake-locs s))))) (- W RADIUS))
         (< (posn-y (tail-position (first (reverse (snake-locs s))))) RADIUS)
         (> (posn-y (tail-position (first (reverse (snake-locs s))))) (- H RADIUS))
         (head-collide? (snake-locs s)))
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
; consumes a snake s and checks if head and edible have collided

(check-expect (edible-collide?
               (make-snake (cons (make-tail (make-posn 200 200) "right") '())
                           (make-posn 300 300)))
              #false)

(check-expect (edible-collide?
               (make-snake (cons (make-tail (make-posn 300 300) "right") '())
                           (make-posn 300 300)))
              #true)

(define (fn-edible-collide? s)
  (cond
    [(equal? (tail-position (first (reverse (snake-locs s))))
             (snake-position s))
     ...]
    [else ...]))

(define (edible-collide? s)
  (cond
    [(equal? (tail-position (first (reverse (snake-locs s))))
             (snake-position s))
     #true]
    [else #false]))

; Snake -> LoCS
; Consumes a snake, moves the head where the edible was, and extends the
; tail by one segment

(check-expect (grow
               (make-snake
                (cons (make-tail (make-posn 200 200) "right") '())
                (make-posn 220 200)))
              (cons (make-tail (make-posn 200 200) "right")
                    (cons (make-tail (make-posn 220 200) "right")
                          '())))

(define (fn-grow s)
  (... (snake-locs s)
       (... (first (reverse (snake-locs s))))))

(define (grow s)
  (append (snake-locs s)
          (create-head (first (reverse (snake-locs s))))))

; Snake -> Image
; consumes a Snake and renders the final world to the screen if last-world
; is true

(check-expect (last-picture
               (make-snake (cons (make-tail (make-posn 200 200) "right") '())
                           (make-posn 100 100)))
              (place-image MSG (/ W 2) (/ H 2)
                           (render-world
                            (make-snake
                             (cons (make-tail (make-posn 200 200) "right") '())
                             (make-posn 100 100)))))
              
(define (fn-last-picture s)
  (... MSG ... ... (render-world s)))

(define (last-picture s)
  (place-image MSG (/ W 2) (/ H 2)
               (render-world s)))

; Snake Posn -> Boolean
; consumes the edible's coordinates and coordinates for the next edible candidate,
; returns true if there is an overlap between the canditate and the snakes
; body.

(check-expect (edible-segment-overlap?
               (make-snake (cons (make-tail (make-posn 200 200) "")
                                 (cons (make-tail (make-posn 220 200) "")
                                       '()))
                           (make-posn 100 100))
               (make-posn 200 200))
              #true)

(check-expect (edible-segment-overlap?
               (make-snake (cons (make-tail (make-posn 200 200) "")
                                 (cons (make-tail (make-posn 220 200) "")
                                       '()))
                           (make-posn 100 100))
               (make-posn 150 150))
              #false)

(define (edible-segment-overlap? s candidate) #t)

; Posn -> Posn 
; consumes coordinates of edible and outputs a different set of coordinates
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
   p (make-posn (random MAX) (random MAX))))
 
; Posn Posn -> Posn 
; generative recursion 
; checks if the current edible coordinates are the same as the newly
; generated set, or a member of the list of connected segments of the snake.
; If so, creates a new set to be checked again.
(define (food-check-create p candidate)
  (if (equal? p candidate)
      (food-create p)
      candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))



; Snake -> Snake
; launches the program from some initial state s

(define (snake-main rate)
  (big-bang SNAKE3
    [on-tick tock rate]
    [to-draw render-world]
    [on-key control-connected]
    [stop-when last-world-connected? last-picture]
    [state #t]))

; --usage
(snake-main 0.5)






