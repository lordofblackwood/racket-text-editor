;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname text_editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)

;; Constants
; Scene
(define SCENE-WIDTH 500)
(define SCENE-HEIGHT 40)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))
; Text
(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define TEXT-LPADDING -10)
; Cursor
(define CURSOR-IMG (rectangle 1 TEXT-SIZE "solid" "red"))

;; Data definitions
(define-struct editor [pre post])
; Editor = (make-editor String String)
; interpretation (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t

(define ed1 (make-editor "Hello" " World"))
(define ed2 (make-editor "GoodBye" "..."))

#;
(define (fn-for-editor ed)
  (...
   (editor-pre ed)
   (editor-post ed)))
; Template rules used:
; - compound: 2 fields

; Editor -> Image
; Consumes an Editor and produces an image with the content
; of the pre and post string properties of it. Also, it places
; the cursor image between the pre and post strings.
(check-expect (render ed1)
              (overlay/align/offset "left" "center"
                                    (place-image
                                     CURSOR-IMG
                                     (cursor-rel-pos ed1) 
                                     (/ (image-height (create-text-image ed1)) 2)
                                     (create-text-image ed1))
                                    TEXT-LPADDING
                                    0
                                    SCENE))

; Template from Editor
(define (render ed)
  (overlay/align/offset "left" "center"
                        (place-image
                         CURSOR-IMG
                         (cursor-rel-pos ed)
                         (/ (image-height (create-text-image ed)) 2)
                         (create-text-image ed))
                        TEXT-LPADDING
                        0
                        SCENE))

; Editor -> Editor
; Consumes an editor and a key stroke and add the letter
; where the cursor is. When the key pressed is the backspace,
; the last letter from pre property is deleted.

(check-expect (editor-key-handler ed1 "w")
              (make-editor (string-append (editor-pre ed1) "w") (editor-post ed1))) 

(check-expect (editor-key-handler ed1 " ")
              (make-editor (string-append (editor-pre ed1) " ") (editor-post ed1)))

(check-expect (editor-key-handler ed1 "\b")
              (make-editor (remove-last-char (editor-pre ed1))
                           (editor-post ed1)))

(check-expect (editor-key-handler ed1 "left")
              (make-editor (remove-last-char (editor-pre ed1))
                           (string-append (last-char (editor-pre ed1)) (editor-post ed1))))

(check-expect (editor-key-handler ed1 "right")
              (make-editor (string-append (editor-pre ed1) (first-char (editor-post ed1)))
                           (string-append (remove-first-char (editor-post ed1))))) 

; (define (editor-key-handler ed key) ed1) ; stub

(define (editor-key-handler ed key)
  (cond [(key=? key "\b") (make-editor
                           (remove-last-char (editor-pre ed))
                           (editor-post ed))]
        [(key=? key "left") (make-editor
                             (remove-last-char (editor-pre ed))
                             (string-append (last-char (editor-pre ed)) (editor-post ed)))]
        [(key=? key "right") (make-editor
                              (string-append (editor-pre ed) (first-char (editor-post ed)))
                              (string-append (remove-first-char (editor-post ed))))]
        [else (make-editor
               (string-append (editor-pre ed) key)
               (editor-post ed))]))

; String -> String
; Consume a string a returns the same string without the last
; character.
(check-expect (remove-last-char "Hello") "Hell")
(check-expect (remove-last-char "") "")

; (define (remove-last-char str) "") ; stub

(define (remove-last-char str)
  (if (> (string-length str) 0)
      (substring str 0 (- (string-length str) 1))
      str))

; String -> String
; Consume a string a returns the same string without the first
; character.
(check-expect (remove-first-char "Hello") "ello")
(check-expect (remove-first-char "") "")

; (define (remove-first-char str) "") ; stub

(define (remove-first-char str)
  (if (> (string-length str) 0)
      (substring str 1 (string-length str))
      str))

; String -> String
; Consume a string and produce the last character of the string.
(check-expect (last-char "Hello") "o")
(check-expect (last-char "") "")

; (define (last-char str) "") ; stub

(define (last-char str)
  (if (> (string-length str) 0)
      (substring str (- (string-length str) 1) (string-length str))
      str))

; String -> String
; Consume a string and produce the first character of the string.
(check-expect (first-char "Hello") "H")
(check-expect (first-char "") "")

; (define (first-char str) "") ; stub

(define (first-char str)
  (if (> (string-length str) 0)
      (string-ith str 0)
      str))

; Editor -> Number
; Consume an editor and produce the position of the cursor based on the
; size of the text image (using only the pre part of the editor).

(check-expect (cursor-rel-pos (make-editor "xx" "qw"))
              (image-width (create-text-image (make-editor "xx" ""))))
(check-expect (cursor-rel-pos (make-editor "" "qw"))
              (image-width (create-text-image (make-editor "" ""))))
(check-expect (cursor-rel-pos (make-editor "" "")) 0)
(check-expect (cursor-rel-pos (make-editor "qwe" ""))
              (image-width (create-text-image (make-editor "qwe" ""))))

; (define (cursor-rel-pos ed) 0) ; stub

(define (cursor-rel-pos ed)
  (if (= (string-length (editor-pre ed)) 0)
      0
      (image-width (create-text-image (make-editor (editor-pre ed) "")))))

; Editor -> Image
; Consume an editor and produce a text image with the string
; contained in the pre an post properties.

(check-expect (create-text-image (make-editor "Hello" " Renzo"))
              (text "Hello Renzo" TEXT-SIZE TEXT-COLOR))

; (define (create-text-image ed) (text "AA" TEXT-SIZE TEXT-COLOR)) ; stub

(define (create-text-image ed)
  (text (string-append (editor-pre ed) (editor-post ed)) TEXT-SIZE TEXT-COLOR))

; Big-Bang
(define (main editor)
  (big-bang editor
            (to-draw render)
            (on-key editor-key-handler)))

(main (make-editor "" ""))