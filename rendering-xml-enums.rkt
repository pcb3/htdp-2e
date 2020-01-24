;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rendering-xml-enums) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================
;; 22.2 Rendering XML Enumerations

(require 2htdp/abstraction)

;;==============
;; 370

; An XWord is '(word ((text String)))

(define XWORD0 '(word ((text "hello"))))
(define XWORD1 '(word ((text "world"))))

; String XWord -> Boolean
; consumes a String s and an XWord xw and produces
; true if xw contains s

(check-expect (word? "hello" XWORD0) #true)
(check-expect (word? "yolo" XWORD0) #false)

(define (fn-word? s xw)
  (local
    ((define fn-extract-content (rest xw)))
    (cond
      [(empty? (fn-extract-content)) ...]
      [else
       (local
         ((define fn-list-of-words
            (first fn-extract-content)))
         (contains-word? s fn-list-of-words))])))

(define (word? s xw)
  (local
    ((define extract-content (rest xw)))
    (cond
      [(empty? extract-content) #false]
      [else
       (local
         ((define list-of-words
            (first extract-content)))
         (contains-word? s list-of-words))])))

; List-of XWord 
(define (contains-word? str low)
  (cond
    [(empty? low) #false]
    [else (if (and (symbol=? (first (first low))
                             'text)
                   (string=? (second (first low))
                             str))
              #true
              (contains-word? str (rest low)))]))
             
