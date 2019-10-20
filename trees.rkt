;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; IV Intertwined Data

; 19 The Poetry of S-expressions

; 19.1 Trees

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field

(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

; Exercise 310

; FT -> Number
; consumes an-ftree and counts the child structures
; in the tree

(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(check-expect (count-persons Eva) 3)

(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else
     (+ 1
        (count-persons (child-father an-ftree))
        (count-persons (child-mother an-ftree)))]))

; Exercise 311

; FT Number -> Number
; consumes a family tree an-ftree and the current year
; and produces the average age of all child structures

(check-expect (average-age Carl 2019) 93)
(check-expect (average-age Gustav 2019) 64.8)

(define (fn-average-age an-ftree year)
  (cond
    [(no-parent? an-ftree) ...]
    [else (... (fn-average-age
                (child-date (child-mother an-ftree)))
               (...
                (fn-average-age
                 (child-date (child-father an-ftree)))
                ...))]))

(define (average-age ftree year)
  (local (
          (define (sum-ages ft)
            (cond
              [(no-parent? ft) 0]
              [else
               (+ (- year (child-date ft))
                  (sum-ages (child-mother ft))
                  (sum-ages (child-father ft)))])))
    (/ (sum-ages ftree)
       (count-persons ftree))))
           
; Exercise 312

; FT -> List-of String
; consumes a family tree ftree and produces a list
; of all the eye colours

(check-expect
 (andmap
  (lambda (x)
    (member x
            (list "brown" "pink" "blue" "green"
                  "green")))
  (eye-colours Gustav)) #true)
                        

(define (fn-eye-colours ftree)
  (cond
    [(no-parent? ftree) ...]
    [else (... (child-eyes ftree)
               (...
                (fn-eye-colours (child-mother ftree))
                (fn-eye-colours (child-father ftree))
                ))]))

(define (eye-colours ftree)
  (cond
    [(no-parent? ftree) '()]
    [else
     (cons (child-eyes ftree)
           (append
            (eye-colours (child-mother ftree))
            (eye-colours (child-father ftree))))]))

; Exercise 313

; FT -> Boolean
; consumes a family tree and produces true if a
; a direct ancestor has blue eyes

(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)

(define (blue-eyed-ancestor? ftree)
  (local
    ((define blue-eyes?
       (lambda (c)
         (string=? "blue" (child-eyes c))))

     (define (tree-father ft)
       (cond
         [(no-parent? (child-father ft)) #false]
         [else
          (if (blue-eyes? (child-father ft))
              #true
              (tree-father (child-father ft)))]))

     (define (tree-mother ft)
       (cond
         [(no-parent? (child-mother ft)) #false]
         [else
          (if (blue-eyes? (child-mother ft))
              #true
              (tree-father (child-mother ft)))])))

    (or (tree-father ftree)
        (tree-mother ftree))))

; 19.2 Forests

; An FF (short for family forest) is one of: 
; – '()
; – (cons FT FF)
; interpretation a family forest represents several
; families (say, a town) and their ancestor trees

(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))

; Exercise 314

; An FF (family forest) is one of:
; - '()
; - (cons [List-of X] [List-of Y])
; interpretation a family forest represents several
; families and their ancestors

; X [List of X] -> Boolean
; consumes an X and produces true if there is a child
; with "blue" eyes

(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)

(define (blue-eyed-child-in-forest? a-forest)
  (ormap blue-eyed-child? a-forest))

; Exercise 315

; FF Number -> Number
; consumes an FF forest and a year yr and
; produces the mean of the ages.
; Not true mean as there will be duplicates

(check-expect (mean-age ff1 2019) 93)
(check-expect (mean-age ff2 2019) 66.5)

(define (mean-age forest yr)
  (local
    ((define forest-length (length forest))

     (define (sum-age-forest fr)
       (cond
         [(empty? fr) 0]
         [else
          (+ (average-age (first fr) yr)
             (sum-age-forest (rest fr)))])))
    (/ (sum-age-forest forest)
       forest-length)))










































