;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rendering-enums-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;=================================
;; rendering-enums-2 

(require 2htdp/image)

;;==================

; An XWord is '(word ((text String)))

(define XWORD0 '(word ((text "hello"))))

(define (word? xw)
  (if (cons? xw)
      (equal? 'word (first xw))
      #false))
  
(define (word-text xw)
  (second (first (first (rest xw)))))

;;==================

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
(define XITEM.V22 `(li ((powerlevel "9000")
                        (salutation "Well met!"))
                       (,XITEM.V20 ,XITEM.V21)))
(define XITEM.V23 `(li ul (,XITEM.V20 ,XITEM.V21)))

(define XENUM.V20 `(ul (,XITEM.V20 ,XITEM.V21)))
(define XENUM.V21 `(ul (,XITEM.V20)))
(define XENUM.V22 `(ul ((status "amber") (load "high"))
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

(check-expect
 (render-enum XENUM.V21)
 (above/align 'left
              (bulletize (text "first" SIZE COLOR))
              empty-image))

(define (render-enum xe)
  (local ((define content (xexpr-content.v2 xe))
          ; XItem.v2 Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))
 
; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect
 (render-item XITEM.V20)
 (bulletize
  (text (word-text (first (xexpr-content.v2 XITEM.V20)))
        SIZE COLOR)))

(check-expect
 (render-item XITEM.V22)
 (above/align
  'left
  (bulletize
   (text (word-text (first (xexpr-content.v2 XITEM.V20)))
         SIZE COLOR))
  (bulletize
   (text (word-text (first (xexpr-content.v2 XITEM.V21)))
         SIZE COLOR))))
              
(define (render-item an-item)
  (local ((define content
            (first (xexpr-content.v2 an-item))))
    (bulletize
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))

;;======================================
;; xexpr-content and list-of-attributes?

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr.v2 -> List-of Xexpr.v2
; consumes an Xexpr.v2 and produces the content


(define (xexpr-content.v2 x)
  (local ((define optional-loa+content (rest x)))
    (cond
      [(empty? optional-loa+content) '()]
      [else (local ((define loa-or-x
                      (first optional-loa+content)))
              (if (list-of-attributes? loa-or-x)
                  (rest optional-loa+content)
                  optional-loa+content))])))
