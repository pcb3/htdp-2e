;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname and-or-map) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 271

; and-, or- ...map!

; String List-of-Strings -> Boolean
; consumes a name and a list of names and determines
; if the name or an extension of the name exists
; in the given list
; (define (find-name s los) #false)

(check-expect (find-name "" '()) #false)

(check-expect (find-name "a" '("aa" "bb" "vc"))
              #true)

(check-expect (find-name "aabra" '("aa" "bb" "cc"))
              #false)

(check-expect (find-name "pie" '("aa" "bb" "cc"))
              #false)

(check-expect (find-name "matos" '("a" "b" "c"))
              #false)

(define (find-name s los)
  (local (; String List-of-Strings-> Boolean
          ; creates a list of 1Strings of s
          (define (x-str-l l)
            (helper (explode s) l))

          ; List-of-Strings List-of-LoS -> Boolean
          ; returns true if a is a subset of b
          (define (helper a b)
            (cond
              [(empty? a) #true]
              [(empty? b) #false]
              [else
               (if (equal? (first a)
                           (first b))
                   (helper (rest a)
                           (rest b))
                   #false)])))
    (ormap x-str-l (map explode los))))



          
            









