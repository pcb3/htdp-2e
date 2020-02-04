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

;;=====================
;; 372

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 XITEM0)
              (beside/align 'center BT
                            (text "happy" 12 'black)))
(check-expect (render-item1 XITEM1)
              (beside/align 'center BT
                            (text "tranquil" 12 'black)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; the function works by first extracting the content of
; the item and then taking the first element, extracting
; the text and creating a ISL+ text constant and finally
; rendering it.

;;=============
;; render-enum1

(define enum0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; XEnum.v1 -> Image 
; renders a simple enumeration as an image 
(check-expect (render-enum1 enum0) e0-rendered)

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))

;;=================================

; An XItem.v2 is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))

; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define XITEM.V20 '(li (word ((text "first")))))
(define XITEM.V21 '(li (word ((text "second")))))
(define XITEM.V22 `(li (((powerlevel "9000")
                         (salutation "Well met!"))
                        (,XITEM.V20 ,XITEM.V21))))

(define XENUM.V20 `(ul (,XITEM.V20 ,XITEM.V21)))
(define XENUM.V21 `(ul ((status "amber") (load "high"))
                       (,XITEM.V20 ,XITEM.V21)))

;;============================
;; 373

(define SIZE 12) ; font size 
(define COLOR "black") ; font color 
(define BT1 ; a graphical constant 
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))
 
; Image -> Image
; marks item with bullet
(check-expect (bulletize (text "happy" SIZE COLOR))
              (beside/align 'center BT1
                            (text "happy" SIZE COLOR)))

(define (bulletize item)
  (beside/align 'center BT1 item))
 
; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect
 (render-enum XENUM.V20)
 (above/align 'left
              (bulletize (text "first" SIZE COLOR))
              (above/align
               'left
               (bulletize (text "second" SIZE COLOR))
               empty-image)))

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image 
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))



















