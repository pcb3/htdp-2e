;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname intermezzo2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; intermezzo 2
; quote, unquote, quasiquote

(require 2htdp/web-io)

; ex 233

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers 
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

(check-expect `(0 ,@'(1 2 3) 4) (list 0 1 2 3 4))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,@'("carl" " , the great")   1500)
                ("dawn" 2300))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl" " , the great" 1500)
                    (list "dawn" 2300)))

(check-expect `(html
                (body
                 (table ((border "1"))
                        (tr ((width "200"))
                            ,@(make-row '( 1  2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65))))))
              (list 'html
                    (list 'body
                          (list 'table (list (list 'border "1"))
                          (list 'tr (list (list 'width "200"))
                                (list 'td "1")
                                (list 'td "2"))
                          (list 'tr (list (list 'width "200"))
                                (list 'td "99")
                                (list 'td "65"))))))

;; ex 234

(define one-list
  '("DJ Boring: Winona"
    "Melo Ball: One"
    "Justin Bieber: What do you mean"))

; Los -> Los
; consumes a list of strings los, reverses, ranks and re-reverses them
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; Los -> Los
; consumes a list of strings los, and creates a list of ranked lists
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
     (head
       (title ,title)
       (meta ((http-equiv "content-type")
              (content "text-html"))))
     (body
       (h1 ,title)
       (p "I, " ,author ", made this page.")
       (table ((border "1"))
                        (tr ((width "800"))
                            ,@(make-row '( 1  2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65)))))))
              
(show-in-browser
   `(table ((border "1"))
           (tr ((width "200"))
               (cons (td ,(number->string (first (first (ranking one-list)))))
               (td ,@(first (ranking one-list)))))))
 
              