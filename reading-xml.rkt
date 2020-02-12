;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname reading-xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;=======================
;; Reading XML

(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; 
; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

;;============================
;; 384

(define PREFIX "https://www.google.com/finance?q=")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; interpretation: A StockWorld is the price of a stock in
; its change in $USD

(define TSLA0 (make-data 771.28 23.1))

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; retrieves the price and change in price of a stock
          ; co in $USD
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; renders stock data to the screen
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    ; creates the text function to send to
                    ; render
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

;;========================
;; 385

(define TSLA (read-xexpr "tsla.xml"))

;;========================
;; 386

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")

(check-expect
  (get '(meta ((content "+1") (itemprop "lambda"))) "p")
  "no such value")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        "no such value")))

; Xexpr.v3 -> [Maybe String]
; consumes an Xexpr.v3 ex and the name of an attribute
; as a Symbol sy and produces the value of that atribute
; or signals and error

(define META0 'X)
(define META1 "one")
(define META2 1)
(define META3 '(meta ((content "+3") (itemprop "M"))))
(define META4 '(meta ((content "+4") (itemprop "M"))
                     (head ((content "-4")
                            (name "Donnie")))))

(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
(check-expect
 (get-xexpr META0 "I") "no such value")
(check-expect (get-xexpr META3 "M") "+3")
(check-expect (get-xexpr META4 "M") "+4")

;;=======================
;; Xexpr helper functions

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
             loa-or-x '()))])))

(define (find-attr loa s)
  (cond
    [(empty? loa) #false]
    [else
     (if (list? (assq s loa))
         (second (assq s  loa))
         #false)]))

;;=======================

(define (fn-get-xexpr xe sy)
  (cond
    [else
     (if (string=? sy (find-attr (xexpr-attr xe)
                                 ...))
              (find-attr (xexpr-attr xe) ...)
              ...)]))
  
(define (get-xexpr xe sy)
  (cond
    [(not (list? xe)) "no such value"]
    [else
     (if (string=? sy (find-attr (xexpr-attr xe)
                                 'itemprop))
              (find-attr (xexpr-attr xe) 'content)
              "no such value")]))























