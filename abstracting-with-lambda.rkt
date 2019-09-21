;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstracting-with-lambda) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 17.3 Abstracting with lambda

; exercise 285

; List-of-Numbers -> List-of-Numbers
; consumes a list of US$ amounts and converts
; them to Euros @ US$1.06 per Euro

(check-expect (convert-euro '()) '())
(check-expect (convert-euro '(1 2 3))
              '(1.06 2.12 3.18))

(define (convert-euro lon)
  (map (lambda (n) (* 1.06 n)) lon))

; List-of-Numbers -> List-of-Numbers
; consumes a list of Fahrenheit measurements and
; produces Celsius measurements

(define (convertFC lon)
  (map (lambda (n) (/ (- n 32) 1.8)) lon))

; List-of-Posns -> List List-of-Numbers
; consumes a list of positions and produces a list
; of lists of number pairs

(check-expect (translate '()) '())
(check-expect (translate (list(make-posn 0 0)
                              (make-posn 1 1)
                              (make-posn 99 100)))
              (list
               (list 0 0) (list 1 1) (list 99 100)))

(define (translate lop)
  (map (lambda (pos) (cons (posn-x pos)
                           (list (posn-y pos))))
       lop))

; exercise 286

(define-struct ir [name desc acq rsp])
(define V1 (make-ir "Boring" "Love's a crime" 9 20))
(define V2 (make-ir "Baltra" "O'Neil" 11 17))
(define V3 (make-ir "Mall Grab" "GUAP" 3 33))
(define VINYL (list V1 V2 V3))

; List-of-ir cmp -> List-of-ir
; consumes a list of iventory records and sorts them
; by the difference in aquistion and sale price
; using cmp

(check-expect (sort-by-diff '()) '())
(check-expect (sort-by-diff VINYL)
              (list V3 V1 V2))
              
(define (sort-by-diff lir)
  (sort lir
        (lambda (ir1 ir2)
          (> (- (ir-rsp ir1) (ir-acq ir1))
             (- (ir-rsp ir2) (ir-acq ir2))))))

; exercise 287

(define-struct inv [name price])
(define INV1 (make-inv "Daikon" 0.99))
(define INV2 (make-inv "Shumeji" 3.99))
(define INV3 (make-inv "Inagi" 6.50))
(define LOR (list INV1 INV2 INV3))

(check-expect (eliminate-exp '() 100) '())
(check-expect (eliminate-exp (list INV1
                                   INV2
                                   INV3) 1)
              (list INV1))

(define (eliminate-exp lor ua)
  (filter (lambda (inv) (< (inv-price inv) ua)) lor)) 


; String List-of-inv -> List-of-inv
; consumes a list of inventory records and produces
; a new list of records excluding record names equal
; to th

(check-expect (recall "Daikon" '()) '())
(check-expect (recall "Daikon" LOR) (list INV2 INV3))

(define (recall ty lor)
  (filter (lambda (inv) (not (equal? (inv-name inv)
                                     ty))) lor))

; List-of-Names List-of-Names -> List-of-Names
; consumes two lists of names and produces a new
; list of every name in both lists

(define (selection lon1 lon2)
  (filter (lambda (x) (member? x lon2)) lon1))

; exercise 288

; Number -> List-of-Numbers

; consumes a Number n and returns a list of numbers
; from 0 to n - 1

(check-expect (build-nat 3) (list 0 1 2))

(define (build-nat n)
  (build-list n (lambda (x) x)))

; consumes a Number n and  produces a list of numbers
; 1 - n

(check-expect (build>0 3) (list 1 2 3))

(define (build>0 n)
  (build-list n (lambda (x) (add1 x))))

; consumes a Number n and produces the list of
; rationals 1 / 1 - 1 / n + 1

(check-expect (build-rational 3)
              (list 1 (/ 1 2) (/ 1 3)))

(define (build-rational n)
  (build-list n (lambda (x) (/ 1 (add1 x)))))

; consumes a Number n and produces the first n even
; numbers

(check-expect (build-even 3) (list 0 2))

(define (build-even n)
  (filter (lambda (y) (even? y))
          (build-list n (lambda (x) x))))

; Nubmer -> List of List-of-Numbers
; consumes a natural Number n and produces the
; n x n identity matrix

(check-expect (/identity 1) (list (list 1)))
(check-expect (/identity 3) (list (list 1 0 0)
                                  (list 0 1 0)
                                  (list 0 0 1)))

(define (/identity n)
  (local (; consumes a Number n and builds a list
          ; with n lists within and 1's on the
          ; diagonal in each list

          (define (fn-build-matrix m)
            (cond
              [(zero? m) ...]
              [else
               (... ...
                (fn-build-matrix (sub1 m)))]))

          (define (build-matrix m)
            (cond
              [(zero? m) '()]
              [else
               (cons
                (reverse
                 (build-list
                  n
                  (lambda (x)
                    (if (= (add1 x) m) 1 0))))
                (build-matrix (sub1 m)))])))
              
    (build-matrix n)))

; [ X Y ] [ X -> Y] -> [List-of Y]
; consumes a Number X and applies a function Y to
; n, n - 1,... 0

(check-expect (tabulate sin 0) (list 0))
(check-expect (tabulate sqr 5)
              (list 25 16 9 4 1 0))

(define (fn-tabulate f n)
  (build-list (... n)
              (lambda (x) (... x))))

(define (tabulate f n)
  (reverse (build-list (add1 n)
                       (lambda (x) (f x)))))

;==
; consumes a Number n a list 1 - n + 1 and produces
; true if all numbers a rational

(andmap (lambda (x) (rational? x))
        (build-list 1000
                    (lambda (x)
                      (/ 1 (add1 x)))))

;==
; identity matrix using lambda from some uknown
; source

; Number -> List-of-LoN
; consumes a number n and returns the identity
; matrix

(define (buidl n)
  (build-list n
              (lambda (i)
                (build-list n
                            (lambda (j)
                              (if (= i j) 1 0))))))

; exercise 289

; String List-of Strings -> Boolean
; consumes a name and a list of names and determines
; if the name or an extension of the name exists
; in the given list

(check-expect (find-name "" '()) #false)

(check-expect (find-name "a" '("aa" "bb" "vc"))
              #true)

(check-expect (find-name "aabra" '("aa" "bb" "cc"))
              #false)

(check-expect (find-name "pie" '("aa" "bb" "cc"))
              #false)

(check-expect (find-name "matos" '("a" "b" "c"))
              #false)

(define (fn-find-name s los)
  (ormap (lambda (x) (... s ...))))

(define (find-name s los)
  (local (; LoS LoS -> Boolean
          ; consumes two lists of strings and
          ; produces true if each letter in the
          ; first string is in the second in order
          
          (define (check-letter los1 los2)
            (cond
              [(empty? los1) #true]
              [(empty? los2) #false]
              [else
               (if (equal? (first los1) (first los2))
                   (check-letter (rest los1)
                                 (rest los2))
                   #false)])))

    (ormap (lambda (l) (check-letter (explode s) l))
           (map explode los))))

; List-of-Strings 1String -> Boolean
; consumes a list of names and a letter and produces
; true if all names start with given letter

(check-expect (starts-with '() "a") #false)

(check-expect
 (starts-with (list "andrew" "andy" "anderson") "a")
 #true)

(check-expect
 (starts-with (list "andrew" "gemma" "anderson") "a")
 #false)

(define (fn-starts-with los 1str)
  (andmap (lambda (x) (equal? (... (... x)) 1str))))

(define (starts-with los 1str)
  (cond
    [(empty? los) #false]
    [else
     (andmap (lambda (x)
               (equal? (first (explode x)) 1str))
             los)]))

; Use ormap. It with return true as soon as the
; first instance of the condition. Its faster!


  




             
















