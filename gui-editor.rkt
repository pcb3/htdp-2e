;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname gui-editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 32.4 A Graphical Editor, with Mouse

(require 2htdp/image)

(define FONT-SIZE 11)
(define FONT-COLOR "black")
 
; [List-of 1String] -> Image
; renders a string as an image for the editor 
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))
 
(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor [List-of 1String] [List-of 1String])
; interpretation if (make-editor p s) is the state of 
; an interactive editor, (reverse p) corresponds to
; the text to the left of the cursor and s to the
; text on the right

;;====
;; 508

; A PosX is one of:
; - at the beginning
; - at the end
; - in-between

; List-of 1Stirng Number -> Editor
; consumes a List of 1String's ed and a natural Number x
; and produces an Editor split at x

;; a line of text
(define img (editor-text (explode "something")))

(check-expect
 (split-structural (explode "something") 28)
 (make-editor (explode "some") (explode "thing")))

(check-expect
 (split-structural (explode "something") 0)
 (make-editor '() (explode "something")))

(check-expect
 (split-structural (explode "something") 999)
 (make-editor (explode "something") '()))

(define (split-structural ed x)
  (local
    ((define (place-between p s)
       (cond
         [(and (empty? s)
               (string=? (implode p) (implode ed))
               (<= (image-width (editor-text p)) x))
          (make-editor p s)]
         [(and (string=? (implode (append p s)) (implode ed))
               (<= (image-width (editor-text p))
                   x
                   (image-width (editor-text
                                 (append p (list (first s)))))))
          (make-editor p s)]
         [else
          (place-between (append p (list (first s))) (rest s))])))

    (place-between '() ed)))









