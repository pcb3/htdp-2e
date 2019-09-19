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


; recall
(check-expect (recall "Daikon" '()) '())
(check-expect (recall "Daikon" LOR) (list INV2 INV3))

(define (recall ty lor)
  (filter (lambda (inv) (not (equal? (inv-name inv)
                                     ty))) lor))
             
















