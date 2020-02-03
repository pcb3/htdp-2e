;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rendering-xml-enums) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================
;; 22.2 Rendering XML Enumerations

(require 2htdp/abstraction)
(require 2htdp/image)


;;==========================
;;

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

;;=======================
;; xexpr-content

; Xexpr.v2 -> List-of Xexpr.v2
; consumes an Xexpr.v2 xe and produces a list of content
; elements
(check-expect
 (xexpr-content '(li ((attr "foo")))) '())

(check-expect
 (xexpr-content '(li ((attr "foo"))
                     (word ((text "mamba")))))
 '((word ((text "mamba")))))

(check-expect
 (xexpr-content '(li (word ((text "mamba")))))
 '((word ((text "mamba")))))

(define (xexpr-content x)
  (local ((define optional-loa+content (rest x)))
    (cond
      [(empty? x) '()]
      [else (if (list-of-attributes?
                 (first optional-loa+content))
                (rest optional-loa+content)
                optional-loa+content)])))

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

; List-of XWord -> Boolean
(define (contains-word? str low)
  (cond
    [(empty? low) #false]
    [else (if (and (symbol=? (first (first low))
                             'text)
                   (string=? (second (first low))
                             str))
              #true
              (contains-word? str (rest low)))]))

; XWord -> String
; consumes an XWord xw and produces the value of
; the only attribute of an instance of XWord

(check-expect (word-text XWORD0) "hello")

(define (fn-word-text xw)
  (local
    ((define fn-extract-element  (rest xw))
     (define
       fn-extract-xwords       
       (first fn-extract-element)))
    (cond
      [else (if (symbol=? 'word (first xw))
                (second (first fn-extract-xwords))
                (error "not a word element"))])))
  
(define (word-text xw)
  (local
    ((define extract-element  (rest xw))
     (define
       extract-xwords       
       (first extract-element)))
    (cond
      [else (if (symbol=? 'word (first xw))
                (second (first extract-xwords))
                (error "not a word element"))])))
         
;;=============================
;; 371

;; an Xexpr is:
;; (cons Symbol XL)

;; an XL is one of:
;; - [List-of Xexpr]
;; - (cons [List-of Attribute] [List-of Xexpr])

;; an Attribute is one of:
;; - (cons Symbol Value)
;; - (cons 'text String)

; An XWord is '(word ((text String)))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attribute [List-of XItem.v1]))

; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attribute (cons XWord '())))

(define ENUM0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define ENUM1
  '(ul
    ((attribute "value"))
    ((li (word ((text "one"))))
     (li (word ((text "two")))))))

(define ENUM2
  '(ul
    ((attribute "value"))
    (((li ((attribute1 "fizz")) (word ((text "one")))))
     (li (word ((text "two")))))))

(define XITEM0
  '(li (word ((text "happy")))))

(define XITEM1
  '(li ((another-attr "value1"))
       (word ((text "tranquil")))))
  

(define BT (circle 10 'solid 'black))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(define (render-item1 i)
  (... (xexpr-content i) ...))






             












