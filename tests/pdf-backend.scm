;;;; Unit tests for the native PDF backend (gg-backend-pdf).
;;;;

(import scheme
        
        (scheme base)
        (chicken base)
        (chicken file)
        (chicken file posix)
        (chicken io)
        (chicken pathname)
        (chicken format)
        test
        pdf
        pdf-deflate
        gg-backend
        gg-backend-pdf
        gg-vge
        gg-primitives-vge)

;;; ========================================================================
;;; Helpers
;;; ========================================================================

(define (read-file path)
  (call-with-input-file path
    (lambda (p)
      (let ((bv (read-bytevector (file-size path) p)))
        (bytevector->byte-string bv (bytevector-length bv))))))

;; Plain substring search; returns #f if not found.
(define (substring-index needle haystack start)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (let loop ((i start))
      (cond ((> (+ i nlen) hlen) #f)
            ((string=? needle (substring haystack i (+ i nlen))) i)
            (else (loop (+ i 1)))))))

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

(define (string-suffix? suffix str)
  (and (>= (string-length str) (string-length suffix))
       (string=? suffix (substring str (- (string-length str) (string-length suffix))
                                   (string-length str)))))

(define (count-occurrences needle haystack)
  (let ((nlen (string-length needle))
        (hlen (string-length haystack)))
    (let loop ((i 0) (n 0))
      (if (> (+ i nlen) hlen)
          n
          (loop (+ i 1)
                (if (string=? needle (substring haystack i (+ i nlen)))
                    (+ n 1)
                    n))))))

;; Parse the run of ASCII digits starting at pos.
(define (parse-uint-at contents pos)
  (let loop ((i pos) (n 0))
    (if (and (< i (string-length contents))
             (char-numeric? (string-ref contents i)))
        (loop (+ i 1)
              (+ (* n 10)
                 (- (char->integer (string-ref contents i))
                    (char->integer #\0))))
        n)))

;; Extracts the bytes of the Nth stream object from a PDF file's
;; contents (assumes streams are written uncompressed).
(define (extract-stream-bytes contents occurrence)
  (let loop ((start 0) (n occurrence))
    (let ((length-pos (substring-index "/Length " contents start)))
      (unless length-pos (error 'extract-stream-bytes "no more streams found"))
      (let* ((digits-start (+ length-pos (string-length "/Length ")))
             (declared-length (parse-uint-at contents digits-start))
             (stream-pos (substring-index "stream\n" contents digits-start)))
        (unless stream-pos
          (error 'extract-stream-bytes "stream keyword not found after /Length"))
        (let ((body-start (+ stream-pos (string-length "stream\n"))))
          (if (= n 0)
              (substring contents body-start (+ body-start declared-length))
              (loop (+ body-start declared-length) (- n 1))))))))

;; Render drawers through a fresh PDF backend at a fixed 200x100
;; canvas and return the whole file text.  Compression is disabled so
;; the content stream can be inspected directly.
(define (render->file drawers #!key (pdf-version "1.4"))
  (let* ((path (create-temporary-file "pdf"))
         (backend (make-pdf-backend path 200 100
                                    #:pdf-version pdf-version
                                    #:compress? #f))
         (vge (make-vge)))
    (for-each (lambda (d) (render-drawer d vge)) drawers)
    (vge-render! vge backend)
    (let ((contents (read-file path)))
      (delete-file path)
      contents)))

;; Render drawers and return just the page's content-stream text.
(define (render->content drawers #!key (pdf-version "1.4"))
  (extract-stream-bytes (render->file drawers #:pdf-version pdf-version) 0))

;;; ========================================================================
;;; Stroked primitives
;;; ========================================================================

(test-group "pdf: stroked primitives"

  (test-assert "pen color then draw-line emits RG, m, l, S"
    (let ((s (render->content
               (list (with-pen-color "red" (line-drawer 0 0 100 50))))))
      (and (substring-index "1.000 0.000 0.000 RG" s 0)
           (substring-index "0.000 0.000 m" s 0)
           (substring-index "100.000 50.000 l" s 0)
           (substring-index " S\n" s 0))))

  (test-assert "polyline emits m, l*, S"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (polyline-drawer (list (cons 10 10)
                                               (cons 20 20)
                                               (cons 30 10))))))))
      (and (substring-index "10.000 10.000 m" s 0)
           (substring-index "20.000 20.000 l" s 0)
           (substring-index "30.000 10.000 l" s 0)
           (substring-index " S\n" s 0))))

  (test-assert "rect emits re then S"
    (let ((s (render->content
               (list (with-pen-color "black" (rect-drawer 10 20 50 30))))))
      (and (substring-index "10.000 20.000 50.000 30.000 re" s 0)
           (substring-index " S\n" s 0))))

  (test-assert "circle emits four Bezier segments"
    (let ((s (render->content
               (list (with-pen-color "black" (circle-drawer 100 50 40))))))
      ;; A full circle splits into 4 cubic segments (4 c operators).
      (= 4 (count-occurrences " c\n" s))))

  (test-assert "polygon closes with h then S"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (polygon-drawer (list (cons 0 0)
                                               (cons 10 0)
                                               (cons 10 10))))))))
      (and (substring-index " h\n" s 0)
           (substring-index " S\n" s 0))))

  (test-assert "empty polyline and polygon emit nothing"
    (let ((s (render->content
               (list (with-pen-color "black" (polyline-drawer '()))
                     (with-pen-color "black" (polygon-drawer '()))))))
      (and (not (substring-index " m\n" s 0))
           (not (substring-index " l\n" s 0))
           (not (substring-index " S\n" s 0)))))

  (test-assert "zero-radius circle emits nothing"
    (let ((s (render->content
               (list (with-pen-color "black" (circle-drawer 50 50 0))))))
      (not (substring-index " c\n" s 0)))))

;;; ========================================================================
;;; Filled primitives
;;; ========================================================================

(test-group "pdf: filled primitives"

  (test-assert "transparent pen suppresses the stroke (re then f)"
    (let ((s (render->content
               (list (with-pen-color "transparent"
                        (with-fill-color "steelblue"
                          (filled-rect-drawer 10 10 50 30)))))))
      (and (substring-index "0.275 0.510 0.706 rg" s 0)
           (substring-index "10.000 10.000 50.000 30.000 re" s 0)
           (substring-index " f\n" s 0)
           (not (substring-index " B\n" s 0)))))

  (test-assert "visible pen and fill emit B (fill-and-stroke)"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-fill-color "red"
                          (filled-rect-drawer 10 10 50 30)))))))
      (substring-index " B\n" s 0)))

  (test-assert "invisible fill strokes only (S)"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-fill-color "transparent"
                          (filled-rect-drawer 10 10 50 30)))))))
      (and (substring-index " S\n" s 0)
           (not (substring-index " f\n" s 0))
           (not (substring-index " B\n" s 0)))))

  (test-assert "filled polygon closes and fills with b"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-fill-color "green"
                          (filled-polygon-drawer
                            (list (cons 0 0) (cons 10 0)
                                  (cons 10 10) (cons 0 10)))))))))
      (substring-index " b\n" s 0))))

;;; ========================================================================
;;; Alpha transparency (PDF 1.4 ExtGState)
;;; ========================================================================

(test-group "pdf: alpha transparency"

  (test-assert "translucent fill emits gs and declares ExtGState with %PDF-1.4"
    (let ((contents (render->file
                      (list (with-pen-color "transparent"
                               (with-fill-color (color:rgba 1.0 0.0 0.0 0.5)
                                 (filled-rect-drawer 10 10 50 30)))))))
      (and (string-prefix? "%PDF-1.4\n" contents)
           (substring-index "/Type /ExtGState" contents 0)
           (substring-index "/ca 0.5 " contents 0)
           (substring-index " gs\n" (extract-stream-bytes contents 0) 0))))

  (test-assert "opaque colors build no ExtGState objects"
    (let ((contents (render->file
                      (list (with-pen-color "black"
                               (with-fill-color "red"
                                 (filled-rect-drawer 10 10 50 30)))))))
      (not (substring-index "/Type /ExtGState" contents 0))))

  (test-assert "pdf-version 1.3 never requests alpha (no gs operator)"
    (let ((contents (render->file
                      (list (with-pen-color "transparent"
                             (with-fill-color (color:rgba 1.0 0.0 0.0 0.5)
                               (filled-rect-drawer 10 10 50 30))))
                      #:pdf-version "1.3")))
      (and (string-prefix? "%PDF-1.3\n" contents)
           (not (substring-index " gs\n"
                                 (extract-stream-bytes contents 0) 0)))))
  )

;;; ========================================================================
;;; State stack and clipping
;;; ========================================================================

(test-group "pdf: state stack"

  (test-assert "group brackets with q/Q"
    (let ((s (render->content
               (list (with-pen-color "red"
                        (with-pen-color "blue"
                          (line-drawer 0 0 10 10)))))))
      (and (substring-index " q\n" s 0)
           (substring-index " Q\n" s 0)
           (= 2 (count-occurrences " q\n" s)))))

  (test-assert "pop-state! underflow raises an error"
    (condition-case
      (let ((b (make-pdf-backend "/tmp/opencode/underflow.pdf" 100 100)))
        (backend/open! b)
        (backend/pop-state! b)
        #f)
      (exn () #t)))

  (test-assert "drawing before open raises an error"
    (condition-case
      (let ((b (make-pdf-backend "/tmp/opencode/notopen.pdf" 100 100)))
        (backend/draw-line! b 0 0 10 10)
        #f)
      (exn () #t))))

(test-group "pdf: clipping"

  (test-assert "clip rect emits re, W, n inside q/Q"
    (let ((s (render->content
               (list (with-clip-rect 10 10 50 30
                        (with-pen-color "black"
                          (line-drawer 0 0 100 100)))))))
      (and (substring-index "10.000 10.000 50.000 30.000 re\n W\n n\n" s 0)
           (substring-index " Q\n" s 0))))

  (test-assert "reset-clip! removes only the clip state (balanced q/Q)"
    (let* ((path (create-temporary-file "pdf"))
           (b (make-pdf-backend path 200 100 #:compress? #f)))
      (backend/open! b)
      (backend/set-clip-rect! b 5 5 40 20)
      (backend/reset-clip! b)
      (backend/set-pen-color! b "black")
      (backend/draw-line! b 0 0 100 100)
      (backend/close! b)
      (let ((s (extract-stream-bytes (read-file path) 0)))
        (delete-file path)
        (and (= 1 (count-occurrences " q\n" s))
             (= 1 (count-occurrences " Q\n" s))
             (substring-index "5.000 5.000 40.000 20.000 re\n W\n n\n" s 0)
             (substring-index "100.000 100.000 l\n S\n" s 0)))))

  (test-assert "unmatched reset-clip! is a no-op"
    (let* ((path (create-temporary-file "pdf"))
           (b (make-pdf-backend path 200 100 #:compress? #f)))
      (backend/open! b)
      (backend/reset-clip! b)
      (backend/close! b)
      (let ((s (extract-stream-bytes (read-file path) 0)))
        (delete-file path)
        (and (not (substring-index " q\n" s 0))
             (not (substring-index " Q\n" s 0)))))))

;;; ========================================================================
;;; Dash pattern
;;; ========================================================================

(test-group "pdf: dash pattern"

  (test-assert "dashes emit the d operator"
    (let ((s (render->content
               (list (with-dash '(10.0 5.0) 0.0
                        (with-pen-color "black"
                          (line-drawer 0 0 10 10)))))))
      (substring-index "[10.0 5.0] 0.0" s 0)))

  (test-assert "empty dash list restores solid ([] 0 d)"
    (let ((s (render->content
               (list (with-dash '() 0.0
                        (with-pen-color "black"
                          (line-drawer 0 0 10 10)))))))
      (substring-index "[] 0.0" s 0))))

;;; ========================================================================
;;; Text
;;; ========================================================================

(test-group "pdf: text"

  (test-assert "text emits BT, Tf, Tm, Tj, ET"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 50 50 "hello"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      (and (substring-index "BT\n" s 0)
           (substring-index " 12.00 Tf\n" s 0)
           (substring-index "(hello) Tj\n" s 0)
           (substring-index "ET\n" s 0))))

  (test-assert "left/baseline alignment puts the anchor at the text origin"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 60 40 "A"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      ;; E/F must be exactly the device anchor: 60.000 40.000
      (substring-index "1.000 0.000 0.000 1.000 60.000 40.000 Tm\n" s 0)))

  (test-assert "center/center alignment shifts E/F by the anchor offset"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 60 40 "AA"
                                       #:halign 'halign/center
                                       #:valign 'valign/center)))))))
      ;; Helvetica 12pt: width("AA") = 2*667*12/1000 = 16.008
      ;; ascent = 718*12/1000 = 8.616, descent = -207*12/1000 = -2.484
      ;; tx = 8.004, ty = (8.616 + -2.484)/2 = 3.066
      ;; E = 60 - 8.004 = 51.996, F = 40 - 3.066 = 36.934
      (substring-index "1.000 0.000 0.000 1.000 51.996 36.934 Tm\n" s 0)))

  (test-assert "rotated text bakes rotation into the Tm matrix"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (with-rotate (/ 3.14159 2)
                            (text-drawer 50 50 "y-axis"
                                         #:halign 'halign/left
                                         #:valign 'valign/baseline))))))))
      (substring-index "0.000 1.000 -1.000 0.000" s 0)))

  (test-assert "parentheses and backslashes in text are escaped"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 10 10 "a(b)c"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      (substring-index "(a\\(b\\)c) Tj\n" s 0)))

  (test-assert "font family mapping selects base-14 fonts"
    (let ((s (render->file
               (list (with-pen-color "black"
                        (with-font "serif" 12.0 'italic 'bold
                          (text-drawer 10 10 "x"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))
                     (with-pen-color "black"
                        (with-font "monospace" 12.0 'normal 'normal
                          (text-drawer 10 50 "x"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))
                     (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 10 80 "x"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      (and (substring-index "/BaseFont /Times-BoldItalic" s 0)
           (substring-index "/BaseFont /Courier" s 0)
           (substring-index "/BaseFont /Helvetica" s 0))))

  (test-assert "unrecognized family falls back to Helvetica"
    (let ((s (render->file
               (list (with-pen-color "black"
                        (with-font "Comic Neue Ultra" 12.0 'normal 'normal
                          (text-drawer 10 10 "x"
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      (substring-index "/BaseFont /Helvetica" s 0)))

  (test-assert "empty string emits no text operators"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 10 10 ""
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      (not (substring-index "BT\n" s 0))))

  (test-assert "text-extents returns AFM width and metrics"
    (let ((b (make-pdf-backend "/tmp/opencode/extents.pdf" 100 100)))
      (backend/open! b)
      (backend/set-font! b "sans-serif" 12.0 'normal 'normal)
      (call-with-values
        (lambda () (backend/text-extents b "AA"))
        (lambda (w h asc desc)
          ;; width = 2*667*12/1000 = 16.008
          ;; ascent 718*12/1000, descent magnitude 207*12/1000
          (and (< (abs (- w 16.008)) 0.01)
               (< (abs (- asc 8.616)) 0.01)
               (< (abs (- desc 2.484)) 0.01)
               (< (abs (- h 11.1)) 0.01))))))

  (test-assert "control characters sanitize to spaces (no crash)"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (with-font "sans-serif" 12.0 'normal 'normal
                          (text-drawer 10 10 (string #\tab #\space)
                                       #:halign 'halign/left
                                       #:valign 'valign/baseline)))))))
      ;; The tab is sanitized to a space, so both glyphs render as
      ;; spaces in the emitted literal string.
      (substring-index "(  ) Tj\n" s 0))))

;;; ========================================================================
;;; General paths
;;; ========================================================================

(test-group "pdf: paths"

  (test-assert "path with move/line/curve/close maps to m, l, c, h"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (path-drawer
                          (list (path:move-to 10 10)
                                (path:line-to 20 20)
                                (path:curve-to 30 10 40 30 50 20)
                                (path:close))))))))
      (and (substring-index "10.000 10.000 m\n" s 0)
           (substring-index "20.000 20.000 l\n" s 0)
           (substring-index "30.000 10.000 40.000 30.000 50.000 20.000 c\n" s 0)
           (substring-index " h\n" s 0))))

  (test-assert "filled path with transparent pen fills without stroking"
    (let ((s (render->content
               (list (with-pen-color "transparent"
                        (with-fill-color "red"
                          (path-drawer
                            (list (path:move-to 0 0)
                                  (path:line-to 30 0)
                                  (path:line-to 30 30)
                                  (path:close))
                            #:filled? #t)))))))
      (and (substring-index " f\n" s 0)
           (not (substring-index " B\n" s 0)))))

  (test-assert "full-circle arc splits into 4 Bezier segments"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (path-drawer
                          (list (path:arc 100 50 40 0.0 6.283185))))))))
      (= 4 (count-occurrences " c\n" s))))

  (test-assert "quarter-circle arc emits a single Bezier segment"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (path-drawer
                          (list (path:arc 50 50 20 0.0 1.5707963))))))))
      (= 1 (count-occurrences " c\n" s))))

  (test-assert "zero-extent arc emits nothing"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (path-drawer
                          (list (path:arc 50 50 20 0.5 0.5))))))))
      (not (substring-index " c\n" s 0))))

  (test-assert "arc without a current point supplies an implicit move-to"
    (let ((s (render->content
               (list (with-pen-color "black"
                        (path-drawer
                          (list (path:arc 50 50 20 0.0 1.5707963))))))))
      (and (= 1 (count-occurrences " m\n" s))
           (= 1 (count-occurrences " c\n" s))
           (substring-index " S\n" s 0)))))

;;; ========================================================================
;;; Viewport and translate
;;; ========================================================================

(test-group "pdf: viewport"

  (test-assert "set-viewport rescales user coordinates"
    (let ((s (render->content
               (list (with-viewport 0 0 200 100
                        (with-pen-color "black"
                          (line-drawer 100 50 200 100)))))))
      (and (substring-index "100.000 50.000 m\n" s 0)
           (substring-index "200.000 100.000 l\n" s 0))))

  (test-assert "non-default viewport maps user rect onto full device"
    (let ((s (render->content
               (list (with-viewport 0 0 2 1
                        (with-pen-color "black"
                          (line-drawer 1 0.5 2 1)))))))
      (and (substring-index "100.000 50.000 m\n" s 0)
           (substring-index "200.000 100.000 l\n" s 0))))

  (test-assert "translate shifts the user origin"
    (let ((s (render->content
               (list (with-translate 50 25
                        (with-pen-color "black"
                          (line-drawer 0 0 10 10)))))))
      (and (substring-index "50.000 25.000 m\n" s 0)
           (substring-index "60.000 35.000 l\n" s 0)))))

;;; ========================================================================
;;; Document structure
;;; ========================================================================

(test-group "pdf: document structure"

  (test-assert "file has header, one page, fonts, xref, trailer, EOF"
    (let ((contents (render->file
                      (list (with-pen-color "black"
                               (with-font "sans-serif" 12.0 'normal 'normal
                                 (text-drawer 10 10 "structural"
                                              #:halign 'halign/left
                                              #:valign 'valign/baseline)))))))
      (and (string-prefix? "%PDF-1.4\n" contents)
           (= 1 (count-occurrences "/Type /Page \n" contents))
           (= 1 (count-occurrences "/Type /Font" contents))
           (substring-index "xref" contents 0)
           (substring-index "trailer" contents 0)
           (string-suffix? "%%EOF\n" contents))))

  (test-assert "close before open is a no-op"
    (condition-case
      (let ((b (make-pdf-backend "/tmp/opencode/noclose.pdf" 100 100)))
        (backend/close! b)
        #t)
      (exn () #f)))

  (test-assert "dimensions report the constructor values"
    (let ((b (make-pdf-backend "/tmp/opencode/dims.pdf" 200 100)))
      (and (= 200.0 (backend/get-width b))
           (= 100.0 (backend/get-height b))))))

(test-group "pdf: compressed output round-trips through FlateDecode"
  (let* ((path (create-temporary-file "pdf"))
         (backend (make-pdf-backend path 200 100 #:compress? #t))
         (vge (make-vge)))
    (render-drawer (with-pen-color "black" (line-drawer 0 0 100 50)) vge)
    (vge-render! vge backend)
    (let* ((contents (read-file path))
           (decompressed (flate-decompress (extract-stream-bytes contents 0))))
      (delete-file path)
      (test-assert "content stream declares FlateDecode"
        (substring-index "/Filter /FlateDecode" contents 0))
      (test-assert "line geometry survives the compression round-trip"
        (substring-index "100.000 50.000 l" decompressed 0)))))

(test-exit)
