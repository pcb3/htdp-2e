;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname commerce-of-xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;================================
;; 22 Project: The Commerce of XML

(require 2htdp/abstraction)
	
; An Xexpr.v2 is a list: 
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

;;====
;; 363

;; an Xexpr.v2 is a list:
;; - (cons Symbol XL)

;; an XL is one of:
;; - '()
;; - Xexpr.v2
;; - (cons Xexpr.v2 XL)
;; - (cons AL (cons Xexpr.v2 XL))

;; an Attribute is:
;; (cons Symbol (cons String '()))

;; an AL is one of:
;; - '()
;; - (cons Attribute AL)

;;====
;; 364

; <transition from="seen-e" to="seen-f" />
(define XEXPR1 '(transition ((from "seen-e") (to "seen-f"))))

; <ul><li><word /><word /></li><li><word /></li></ul>
(define XEXPR2 '(ul (li (word)(word))(li (word))))

;;====
;; 365

; '(server ((name "example.org")))
; <server name ="example.org" />

; '(carcas (board (grass)) (player ((name "sam"))))
; <carcas <board <grass /></board>
; <player name="sam" /></carcas>

; '(start)
; <start />

;;================================

(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

;;================================
;; xepr-attr

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (fn-xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) ...]
      [else (... (first optional-loa+content)
                 ... (rest optional-loa+content) ...)])))

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

;;==============================
;; 366

; Xexpr.v2 -> Symbol
; consumes an Xexpr.v2 xe and produces a Symbol that is
; the tag of the element representation

(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name `(action ,a0)) 'action)

(define (xexpr-name xe) (first xe))

; Xexpr.v2 -> List-of Xexpr.v2
; consumes an Xexpr.v2 xe and produces a list of content
; elements

(check-expect (xexpr-content XEXPR1) '())
(check-expect (xexpr-content XEXPR2)
              '((li (word)(word)) (li (word))))
               
(define (xexpr-content xe) '())















