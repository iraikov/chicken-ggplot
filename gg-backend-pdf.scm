;; gg-backend-pdf.scm
;; Native PDF graphics backend for the gg Grammar of Graphics library,
;; built on the pdf library.
;;
;; Exports a single backend constructor:
;;   (make-pdf-backend "plot.pdf" 800 600)
;;   (make-pdf-backend "plot.pdf" 595 842 #:pdf-version "1.3" #:compress? #f)
;;
;; COORDINATE SYSTEM
;; -----------------
;; PDF pages are natively bottom-left origin with Y increasing upward -
;; the same convention the gg protocol uses - so user-to-device mapping
;; is a plain scale-and-offset with no Y-flip.
;;
;; STATE STACK
;; -----------
;; The PDF content stream has no way to read state back, so a Scheme-side
;; <draw-state> record mirrors everything push/pop must restore (viewport,
;; colors, line width, font, rotation, clip depth).  The content stream's
;; own q/Q pair covers the same state in the file: CTM, colors, dash,
;; line width, text state, clip path, and the current ExtGState (alpha).
;;
;; ALPHA
;; -----
;; The document declares PDF 1.4 by default, which allows constant alpha
;; through ExtGState resources.  Alpha is only paid for when used: while
;; both colors are fully opaque no ExtGState is referenced at all.  When a
;; color carries alpha, the backend lazily builds an ExtGState per
;; (fill-alpha . stroke-alpha) pair and applies it with the gs operator.
;; With pdf-version "1.3" alpha is never requested and colors draw fully
;; opaque.
;;
;; FONTS
;; -----
;; Only the 14 standard Type1 fonts are available (Helvetica, Times,
;; Courier families).  Free-form family strings are matched against
;; those families; anything unrecognized falls back to Helvetica.

(module gg-backend-pdf
        (make-pdf-backend)

  (import scheme
          (chicken base)
          (scheme base)
          (scheme char)
          (chicken pathname)
          srfi-13
          srfi-1
          srfi-69
          datatype
          yasos
          gg-backend
          pdf
          pdf-font)

;;; ================================================================
;;; Viewport record
;;; Stores the user-to-device mapping for the current drawing context.
;;; ================================================================

(define-record-type <viewport>
  (%make-vp x0 y0 x1 y1 dev-w dev-h)
  viewport?
  (x0    vp-x0)
  (y0    vp-y0)
  (x1    vp-x1)
  (y1    vp-y1)
  (dev-w vp-dev-w)
  (dev-h vp-dev-h))

(define (make-identity-viewport w h)
  (%make-vp 0.0 0.0 w h w h))

;; User X -> device X (no Y-flip: PDF is already bottom-left, Y-up).
(define (vp->dx vp ux)
  (* (/ (- ux (vp-x0 vp)) (- (vp-x1 vp) (vp-x0 vp))) (vp-dev-w vp)))

;; User Y -> device Y.
(define (vp->dy vp uy)
  (* (/ (- uy (vp-y0 vp)) (- (vp-y1 vp) (vp-y0 vp))) (vp-dev-h vp)))

;; Scale a user-space length to device units along the X axis.
(define (vp-scale-x vp len)
  (* (/ (abs len) (- (vp-x1 vp) (vp-x0 vp))) (vp-dev-w vp)))

;; Scale a user-space length to device units along the Y axis.
(define (vp-scale-y vp len)
  (* (/ (abs len) (- (vp-y1 vp) (vp-y0 vp))) (vp-dev-h vp)))

;; Shift the user-space origin by (dx, dy): local (0,0) maps to device
;; (dx, dy) as a true affine translate.
(define (vp-translate vp dx dy)
  (%make-vp (- (vp-x0 vp) dx) (- (vp-y0 vp) dy)
            (- (vp-x1 vp) dx) (- (vp-y1 vp) dy)
            (vp-dev-w vp)     (vp-dev-h vp)))

;;; ================================================================
;;; Draw-state record
;;; Everything backend/push-state! and backend/pop-state! must restore.
;;; The content stream cannot be read back, so this record is the only
;;; source of truth for the current drawing properties.
;;; ================================================================

(define-record-type <draw-state>
  (%make-ds viewport pen fill line-width
            font-family font-size font-slant font-weight
            rotation clip-depth)
  draw-state?
  (viewport    ds-viewport    ds-viewport-set!)
  (pen         ds-pen         ds-pen-set!)
  (fill        ds-fill        ds-fill-set!)
  (line-width  ds-line-width  ds-line-width-set!)
  (font-family ds-font-family ds-font-family-set!)
  (font-size   ds-font-size   ds-font-size-set!)
  (font-slant  ds-font-slant  ds-font-slant-set!)
  (font-weight ds-font-weight ds-font-weight-set!)
  (rotation    ds-rotation    ds-rotation-set!)
  ;; Number of graphics states opened by set-clip-rect! but not yet
  ;; closed by reset-clip!.  Copied along with every other field so the
  ;; group q/Q save/restore keeps it consistent with the content stream.
  (clip-depth  ds-clip-depth  ds-clip-depth-set!))

(define (make-default-draw-state w h)
  (%make-ds (make-identity-viewport w h)
            color-black color-white
            1.0
            "sans-serif" 12.0 'normal 'normal
            0.0
            0))

(define (copy-draw-state ds)
  (%make-ds (ds-viewport    ds) (ds-pen    ds) (ds-fill ds)
            (ds-line-width  ds)
            (ds-font-family ds) (ds-font-size ds)
            (ds-font-slant  ds) (ds-font-weight ds)
            (ds-rotation    ds)
            (ds-clip-depth  ds)))

;;; ================================================================
;;; Color helpers
;;; ================================================================

;; Return (values r g b a) for a protocol color, defaulting the alpha
;; component for named colors, which the gg color table always carries
;; as 1.0 or 0.0.
(define (color->rgba c)
  (color->rgba-values c))

;; True when the color would paint nothing (alpha = 0).
(define (color-invisible? c)
  (call-with-values
    (lambda () (color->rgba c))
    (lambda (_ _ _ a) (<= a 0.0))))

;; Return the RGBA components of a protocol color as a plain list.
(define (color->rgba-list c)
  (call-with-values (lambda () (color->rgba c)) list))

;;; ================================================================
;;; Font resolution
;;; ================================================================

;; Ascent and descent per base-14 family, in 1/1000 of the font size
;; (Adobe AFM header values).  Symbol and ZapfDingbats are never
;; selected by the family matcher below.
(define *font-metrics*
  '(("Helvetica" 718 -207)
    ("Times"     683 -217)
    ("Courier"   629 -157)))

(define (family-metrics pdf-family)
  (or (assoc pdf-family *font-metrics*)
      (assoc "Helvetica" *font-metrics*)))

;; Map a free-form family string plus slant/weight onto one of the 14
;; standard PDF Type1 fonts.  "mono" is tested before "sans" and "sans"
;; before "serif" because the later strings are substrings of the
;; earlier ones ("sans-serif" contains "serif").  Unrecognized families
;; fall back to Helvetica.
(define (resolve-font-family family)
  (cond ((string-contains-ci family "mono")  "Courier")
        ((string-contains-ci family "sans")  "Helvetica")
        ((string-contains-ci family "serif") "Times")
        ((string-contains-ci family "times") "Times")
        (else "Helvetica")))

;; Build the full PDF base font name for family + slant + weight.
;; The Times family's plain face is named Times-Roman, and its italic
;; variants use -Italic; sans/mono use -Oblique for the slanted faces.
(define (resolve-pdf-font-name family slant weight)
  (let* ((fam   (resolve-font-family family))
         (ital? (memq slant '(italic oblique)))
         (bold? (eq? weight 'bold))
         (base  (cond ((and ital? bold?)
                       (if (string=? fam "Times")
                           "Times-BoldItalic"
                           (string-append fam "-BoldOblique")))
                      (ital?
                       (if (string=? fam "Times")
                           "Times-Italic"
                           (string-append fam "-Oblique")))
                      (bold?
                       (string-append fam "-Bold"))
                      ((string=? fam "Times")
                       "Times-Roman")
                      (else fam))))
    base))

;; Descent from the AFM table is negative (below the baseline).
(define (font-ascent/pdf pdf-font-name size)
  (let ((m (family-metrics (pdf-name->family pdf-font-name))))
    (* size (/ (cadr m) 1000.0))))

(define (font-descent/pdf pdf-font-name size)
  (let ((m (family-metrics (pdf-name->family pdf-font-name))))
    (* size (/ (caddr m) 1000.0))))

;; Recover the family key from a full base-14 name such as
;; "Helvetica-BoldOblique" for the metrics table lookup.
(define (pdf-name->family name)
  (cond ((string-contains-ci name "Courier")   "Courier")
        ((string-contains-ci name "Helvetica") "Helvetica")
        ((string-contains-ci name "Times")    "Times")
        (else "Helvetica")))

;;; ================================================================
;;; Text sanitizing
;;; ================================================================

;; Replace characters the standard-font width tables cannot address:
;; control characters become spaces, and anything outside the 8-bit
;; WinAnsi range becomes a question mark.  Characters below code 32
;; would otherwise index the width vector out of bounds.
(define (sanitize-text s)
  (if (string-index s (lambda (ch)
                        (let ((n (char->integer ch)))
                          (or (< n 32) (> n 255)))))
      (string-map (lambda (ch)
                    (let ((n (char->integer ch)))
                      (cond ((< n 32) #\space)
                            ((> n 255) #\?)
                            (else ch))))
                  s)
      s))

;; Escape the three characters that are special inside a PDF literal
;; string: backslash, open parenthesis, close parenthesis.
(define (pdf-escape-string s)
  (if (string-index s (lambda (ch) (memv ch '(#\\ #\( #\)))))
      (list->string
        (append-map (lambda (ch)
                      (if (memv ch '(#\\ #\( #\)))
                          (list #\\ ch)
                          (list ch)))
                    (string->list s)))
      s))

;;; ================================================================
;;; Arc-to-Bezier conversion
;;; ================================================================

;; Compute the single cubic Bezier segment approximating an arc of the
;; given extent (radians) starting at angle start, centered at
;; (cx, cy).  Returns the two control points and the end point.
(define (bezarc cx cy r start extent)
  (let* ((end (+ start extent))
         (s-start (sin start)) (c-start (cos start))
         (s-end (sin end))     (c-end (cos end))
         (ang/2 (/ extent 2.0))
         (kappa (* (/ 4.0 3.0)
                   (/ (- 1 (cos ang/2))
                      (sin ang/2))))
         (x1 (- c-start (* kappa s-start)))
         (y1 (+ s-start (* kappa c-start)))
         (x2 (+ c-end   (* kappa s-end)))
         (y2 (- s-end   (* kappa c-end))))
    (values (+ (* x1 r) cx) (+ (* y1 r) cy)
            (+ (* x2 r) cx) (+ (* y2 r) cy)
            (+ (* c-end r) cx) (+ (* s-end r) cy))))

;; Full-precision pi, used for the arc splitting threshold and the
;; circle construction.  The pdf egg's own +2pi+ constant is only five
;; significant digits, which would mis-split extents near the 90
;; degree boundary.
(define pdf-pi 3.141592653589793)

;; Emit an arc as one or more cubic Bezier segments, splitting spans
;; wider than 90 degrees so each segment stays within the kappa
;; approximation's error bound.  The boundary comparison carries a tiny
;; tolerance so an extent of exactly 90 degrees is approximated with a
;; single segment instead of being split again by representation noise.
;; A zero extent emits nothing.
(define (pdf-arc-to! pctx cx cy r start extent)
  (unless (zero? extent)
    (if (<= (abs extent) (+ (/ pdf-pi 2.0) 1e-9))
        (let-values (((x1 y1 x2 y2 x3 y3) (bezarc cx cy r start extent)))
          (bezier-to pctx x1 y1 x2 y2 x3 y3))
        (let ((half (/ extent 2.0)))
          (pdf-arc-to! pctx cx cy r start half)
          (pdf-arc-to! pctx cx cy r (+ start half) half)))))

;;; ================================================================
;;; Path building
;;; ================================================================

;; Translate a list of path-cmd values into PDF path operators.  All
;; coordinates are mapped from user space to device space through vp.
;; Returns #t if the built path contains a current subpath (at least one
;; move-to), so callers know whether a paint operator is meaningful.
(define (pdf-build-path! pctx vp cmds)
  (let ((has-subpath? #f))
    (for-each
      (lambda (cmd)
        (cases path-cmd cmd
          (path:move-to (x y)
           (move-to pctx (vp->dx vp x) (vp->dy vp y))
           (set! has-subpath? #t))
          (path:line-to (x y)
           ;; A path must start with a move-to; supply one when the
           ;; command list begins with line-to (or follows only arcs).
           (unless has-subpath?
             (move-to pctx (vp->dx vp x) (vp->dy vp y))
             (set! has-subpath? #t))
           (line-to pctx (vp->dx vp x) (vp->dy vp y)))
          (path:curve-to (x1 y1 x2 y2 x3 y3)
           (unless has-subpath?
             (move-to pctx (vp->dx vp x1) (vp->dy vp y1))
             (set! has-subpath? #t))
           (bezier-to pctx
                      (vp->dx vp x1) (vp->dy vp y1)
                      (vp->dx vp x2) (vp->dy vp y2)
                      (vp->dx vp x3) (vp->dy vp y3)))
          (path:arc (cx cy r a1 a2)
           ;; Angles follow the user convention (CCW, Y-up), which is
           ;; the native PDF convention too - no negation needed.
           (let ((dx  (vp->dx vp cx))
                 (dy  (vp->dy vp cy))
                 (dr  (vp-scale-x vp r)))
             (unless (<= dr 0.0)
               (unless has-subpath?
                 (move-to pctx (+ dx (* dr (cos a1))) (+ dy (* dr (sin a1))))
                 (set! has-subpath? #t))
               (pdf-arc-to! pctx dx dy dr a1 (- a2 a1)))))
          (path:close ()
           (when has-subpath?
             (close-path pctx)))))
      cmds)
    has-subpath?))

;;; ================================================================
;;; Backend factory
;;; ================================================================

;; width/height are PDF points (72 per inch).  pdf-version selects
;; between "1.4" (default; enables ExtGState alpha) and strict "1.3"
;; (alpha never requested - colors draw fully opaque).  compress?
;; controls FlateDecode compression of the page content stream.
(define (make-pdf-backend filename width height
                          #!key (pdf-version "1.4") (compress? #t))
  (let* ((w*   (exact->inexact width))
         (h*   (exact->inexact height))
         (doc-cell   (list #f))   ;; mutable cell: #f or doc record
         (pctx-cell  (list #f))   ;; mutable cell: #f or page-context
         (port-cell  (list #f))   ;; mutable cell: #f or string output port
         (ds-cell    (list #f))   ;; mutable cell: #f or <draw-state>
         (stack      (list '()))) ;; mutable cell: list of saved draw-states
         ;; Cache of registered font objects keyed by PDF base font
         ;; name.  Font objects are size-independent; only the Tf size
         ;; argument varies per draw call.
         ;; Cache of ExtGState resources keyed by (fill-alpha . stroke-alpha).
    (define font-cache (make-hash-table string=?))
    (define gstate-cache (make-hash-table equal?))

    (define (doc)  (car doc-cell))
    (define (pctx) (car pctx-cell))
    (define (oport) (car port-cell))
    (define (ds)   (car ds-cell))

    (define (alpha-supported?)
      (string=? (doc-pdf-version (doc)) "1.4"))

    ;; Return the registered font object for a PDF base font name,
    ;; building and caching it on first use.
    (define (font-obj/ensure! name)
      (or (hash-table-ref/default font-cache name #f)
          (let ((obj (build-font (doc) name)))
            (hash-table-set! font-cache name obj)
            obj)))

    ;; Return the ExtGState resource name for the given alpha pair,
    ;; building and caching it on first use.  Only called when at least
    ;; one alpha is fractional and the document is PDF 1.4.
    (define (gstate/ensure! fa sa)
      (let ((key (cons fa sa)))
        (or (hash-table-ref/default gstate-cache key #f)
            (let ((gs (build-ext-gstate (doc) fa sa)))
              (hash-table-set! gstate-cache key gs)
              gs))))

    ;; Emit the current stroke color as an RG operator, followed by the
    ;; fill color as rg.  When either color carries a fractional alpha,
    ;; also apply the matching ExtGState so subsequent paint operators
    ;; blend correctly.  In a PDF 1.3 document alpha is never applied
    ;; and colors draw fully opaque.
    (define (emit-colors! pen fill)
      (let ((pr (color->rgba-list pen))
            (fr (color->rgba-list fill)))
        (let ((fa (cadddr fr))
              (sa (cadddr pr)))
          (when (and (or (and (> fa 0.0) (< fa 1.0))
                         (and (> sa 0.0) (< sa 1.0)))
                     (alpha-supported?))
            (let ((gs (gstate/ensure! fa sa)))
              (set-ext-gstate (pctx) (ext-gstate-name gs))))
          (set-rgb-stroke (pctx) (car pr) (cadr pr) (caddr pr))
          (set-rgb-fill   (pctx) (car fr) (cadr fr) (caddr fr)))))

    ;; Emit stroke color only, with its ExtGState when alpha is
    ;; fractional.  Used by stroke-only primitives.
    (define (emit-stroke-color! pen)
      (let ((pr (color->rgba-list pen)))
        (let ((sa (cadddr pr)))
          (when (and (> sa 0.0) (< sa 1.0) (alpha-supported?))
            (let ((gs (gstate/ensure! 1.0 sa)))
              (set-ext-gstate (pctx) (ext-gstate-name gs))))
          (set-rgb-stroke (pctx) (car pr) (cadr pr) (caddr pr)))))

    (define (ensure-open!)
      (unless (pctx)
        (error "make-pdf-backend: backend not opened; call backend/open! first")))

    (object
      ((graphics-backend? self) #t)

      ;; Lifecycle

      ((backend/open! self)
       (let ((d    (build-doc compress? pdf-version))
             (port (open-output-string)))
         (set-car! doc-cell  d)
         (set-car! port-cell port)
         (set-car! pctx-cell (build-page-context port width height))
         (set-car! ds-cell   (make-default-draw-state w* h*))
         (set-car! stack     '())
         (hash-table-clear! font-cache)
         (hash-table-clear! gstate-cache)))

      ((backend/close! self)
       ;; Closing before opening is a no-op, matching the Cairo backend.
       (when (pctx)
         (let* ((stream-obj (build-pdf-stream (get-output-string (oport))))
                (content     (build-indirect-obj (doc) stream-obj))
                (page        (build-page (doc) width height content)))
           (add-page (doc) page)
           (write-document (doc) filename))
         (set-car! doc-cell  #f)
         (set-car! pctx-cell #f)
         (set-car! port-cell #f)
         (set-car! ds-cell   #f)))

      ;; State stack
      ;;
      ;; q/Q save and restore the content-stream graphics state (CTM,
      ;; colors, dash, line width, text state, clip, ExtGState).  The
      ;; Scheme-side draw-state stack saves the fields q/Q cannot be
      ;; read back for: viewport, rotation, font selection, clip depth.

      ((backend/push-state! self)
       (ensure-open!)
       (save-graphics-state (pctx))
       (set-car! stack (cons (copy-draw-state (ds)) (car stack))))

      ((backend/pop-state! self)
       (ensure-open!)
       (when (null? (car stack))
         (error "backend/pop-state!: state stack underflow"))
       (restore-graphics-state (pctx))
       (set-car! ds-cell (car (car stack)))
       (set-car! stack   (cdr (car stack))))

      ;; Viewport

      ((backend/set-viewport! self x0 y0 x1 y1)
       (ensure-open!)
       (ds-viewport-set! (ds)
         (%make-vp (exact->inexact x0) (exact->inexact y0)
                   (exact->inexact x1) (exact->inexact y1)
                   w* h*)))

      ((backend/translate! self dx dy)
       (ensure-open!)
       (ds-viewport-set! (ds)
         (vp-translate (ds-viewport (ds))
                        (exact->inexact dx)
                        (exact->inexact dy))))

      ;; Clipping
      ;;
      ;; set-clip-rect! opens its own graphics state (q) before building
      ;; the clip path so a later reset-clip! (Q) removes exactly this
      ;; clip without disturbing anything the enclosing scope saved.
      ;; clip-depth counts open clip states; the group q/Q stack keeps
      ;; the count consistent because push/pop copy the whole record.

      ((backend/set-clip-rect! self x y w h)
       (ensure-open!)
       (let* ((vp (ds-viewport (ds)))
              (dx (vp->dx vp x))
              (dy (vp->dy vp y))
              (dw (vp-scale-x vp w))
              (dh (vp-scale-y vp h)))
         (save-graphics-state (pctx))
         (basic-rect (pctx) dx dy dw dh)
         (clip-path (pctx))
         (end-path-no-op (pctx))
         (ds-clip-depth-set! (ds) (+ 1 (ds-clip-depth (ds))))))

      ((backend/reset-clip! self)
       (ensure-open!)
       (when (> (ds-clip-depth (ds)) 0)
         (restore-graphics-state (pctx))
         (ds-clip-depth-set! (ds) (- (ds-clip-depth (ds)) 1))))

      ;; Style

      ((backend/set-pen-color! self color)
       (ensure-open!)
       (ds-pen-set! (ds) (parse-color color)))

      ((backend/set-fill-color! self color)
       (ensure-open!)
       (ds-fill-set! (ds) (parse-color color)))

      ((backend/set-line-width! self w)
       (ensure-open!)
       (let ((w* (exact->inexact w)))
         (ds-line-width-set! (ds) w*)
         (set-line-width (pctx) w*)))

      ;; dashes = list of alternating on/off lengths in device pixels;
      ;; the empty list restores a solid line ("[] 0 d" is a valid solid
      ;; pattern in PDF).
      ((backend/set-dash! self dashes offset)
       (ensure-open!)
       (set-dash-pattern (pctx)
                         (map exact->inexact dashes)
                         (exact->inexact offset)))

      ((backend/set-font! self family size slant weight)
       (ensure-open!)
       (ds-font-family-set! (ds) family)
       (ds-font-size-set!   (ds) (exact->inexact size))
       (ds-font-slant-set!  (ds) slant)
       (ds-font-weight-set! (ds) weight))

      ((backend/set-rotation! self angle)
       (ensure-open!)
       (ds-rotation-set! (ds) (exact->inexact angle)))

      ;; Stroked primitives

      ((backend/draw-line! self x1 y1 x2 y2)
       (ensure-open!)
       (let ((vp (ds-viewport (ds))))
         (emit-stroke-color! (ds-pen (ds)))
         (move-to (pctx) (vp->dx vp x1) (vp->dy vp y1))
         (line-to (pctx) (vp->dx vp x2) (vp->dy vp y2))
         (stroke (pctx))))

      ((backend/draw-polyline! self pts)
       (ensure-open!)
       (when (pair? pts)
          (let ((vp (ds-viewport (ds))))
            (emit-stroke-color! (ds-pen (ds)))
            (move-to (pctx)
                     (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
            (for-each
              (lambda (pt)
                (line-to (pctx)
                         (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
              (cdr pts))
            (stroke (pctx)))))

      ((backend/draw-rect! self x y w h)
       (ensure-open!)
       (let* ((vp (ds-viewport (ds)))
              (dx (vp->dx vp x))
              (dy (vp->dy vp y))
              (dw (vp-scale-x vp w))
              (dh (vp-scale-y vp h)))
         (emit-stroke-color! (ds-pen (ds)))
         (basic-rect (pctx) dx dy dw dh)
         (stroke (pctx))))

      ((backend/draw-circle! self cx cy r)
       (ensure-open!)
       (let ((dr (vp-scale-x (ds-viewport (ds)) r)))
          (when (> dr 0.0)
            (let* ((vp (ds-viewport (ds)))
                   (dx (vp->dx vp cx))
                   (dy (vp->dy vp cy)))
              (emit-stroke-color! (ds-pen (ds)))
              (move-to (pctx) (+ dx dr) dy)
              (pdf-arc-to! (pctx) dx dy dr 0.0 (* 2.0 pdf-pi))
              (stroke (pctx))))))

      ((backend/draw-polygon! self pts)
       (ensure-open!)
       (when (pair? pts)
         (let ((vp (ds-viewport (ds))))
           (emit-stroke-color! (ds-pen (ds)))
           (move-to (pctx)
                    (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
           (for-each
             (lambda (pt)
               (line-to (pctx)
                        (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
             (cdr pts))
           (close-path (pctx))
           (stroke (pctx)))))

      ;; Filled primitives
      ;;
      ;; The gg idiom fills with the fill color then strokes the outline
      ;; with the pen color; a transparent pen suppresses the outline.
      ;; B (fill-and-stroke) covers the general case in one operator.
      ;; When the pen is invisible only f is emitted, which avoids a
      ;; pointless ExtGState reference for a stroke that paints
      ;; nothing.  When the fill is invisible only the outline strokes
      ;; (S) - a zero-alpha fill would still paint over the background
      ;; with a B operator.

      ((backend/draw-filled-rect! self x y w h)
       (ensure-open!)
       (let* ((vp (ds-viewport (ds)))
              (dx (vp->dx vp x))
              (dy (vp->dy vp y))
              (dw (vp-scale-x vp w))
              (dh (vp-scale-y vp h))
              (pen  (ds-pen (ds)))
              (fill (ds-fill (ds))))
         (emit-colors! pen fill)
         (basic-rect (pctx) dx dy dw dh)
         (cond ((color-invisible? pen)  (fill-path (pctx)))
               ((color-invisible? fill) (stroke (pctx)))
               (else (fill-and-stroke (pctx))))))

      ((backend/draw-filled-circle! self cx cy r)
       (ensure-open!)
       (let ((dr (vp-scale-x (ds-viewport (ds)) r)))
         (when (> dr 0.0)
           (let* ((vp (ds-viewport (ds)))
                  (dx (vp->dx vp cx))
                  (dy (vp->dy vp cy))
                  (pen  (ds-pen (ds)))
                  (fill (ds-fill (ds))))
             (emit-colors! pen fill)
             (move-to (pctx) (+ dx dr) dy)
             (pdf-arc-to! (pctx) dx dy dr 0.0 (* 2.0 pdf-pi))
             (cond ((color-invisible? pen)  (fill-path (pctx)))
                   ((color-invisible? fill) (stroke (pctx)))
                   (else (close-fill-and-stroke (pctx))))))))

      ((backend/draw-filled-polygon! self pts)
       (ensure-open!)
       (when (pair? pts)
         (let* ((vp   (ds-viewport (ds)))
                (pen  (ds-pen (ds)))
                (fill (ds-fill (ds))))
           (emit-colors! pen fill)
           (move-to (pctx)
                    (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
           (for-each
             (lambda (pt)
               (line-to (pctx)
                        (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
             (cdr pts))
           (cond ((color-invisible? pen)  (close-and-fill (pctx)))
                 ((color-invisible? fill) (close-and-stroke (pctx)))
                 (else (close-fill-and-stroke (pctx)))))))

      ;; General path

      ((backend/draw-path! self cmds filled?)
       (ensure-open!)
       (let ((pen  (ds-pen (ds)))
              (fill (ds-fill (ds))))
         (when (pdf-build-path! (pctx) (ds-viewport (ds)) cmds)
           (cond (filled?
                  (emit-colors! pen fill)
                  (cond ((color-invisible? pen)  (fill-path (pctx)))
                        ((color-invisible? fill) (stroke (pctx)))
                        (else (fill-and-stroke (pctx)))))
                 (else
                  (emit-stroke-color! pen)
                  (stroke (pctx)))))))

      ;; Text
      ;;
      ;; The gg protocol takes the text color from the pen.  Width comes
      ;; from the AFM tables (pdf-font); vertical alignment from the
      ;; ascent/descent table.  Rotation is baked into the text matrix
      ;; (Tm): the matrix is chosen so that the text-space point the
      ;; alignment selects (start/middle/end horizontally, box top /
      ;; center / bottom / baseline vertically) lands exactly on the
      ;; user-space anchor.  With Tm = [a b c d e f], text point (tx,ty)
      ;; maps to device (a.tx + c.ty + e, b.tx + d.ty + f), so e/f are
      ;; the anchor minus the rotated anchor-in-text-space offset.

      ((backend/draw-text! self x y text halign valign)
       (ensure-open!)
       (let* ((vp      (ds-viewport (ds)))
              (size    (ds-font-size (ds)))
              (pdfname (resolve-pdf-font-name (ds-font-family (ds))
                                              (ds-font-slant  (ds))
                                              (ds-font-weight (ds)))))
          (when (and (> size 0.0) (> (string-length text) 0))
            (let* ((clean  (sanitize-text text))
                   (width  (get-string-width pdfname size clean))
                  (asc    (font-ascent/pdf  pdfname size))
                  (desc   (font-descent/pdf pdfname size))
                  ;; Anchor position in text space: horizontal.
                  (tx     (case halign
                            ((halign/center) (/ width 2.0))
                            ((halign/right)  width)
                            (else            0.0)))
                  ;; Anchor position in text space: vertical.  The glyph
                  ;; box spans desc (below baseline, negative) to asc.
                  (ty     (case valign
                            ((valign/top)    asc)
                            ((valign/bottom) desc)
                            ((valign/center) (/ (+ asc desc) 2.0))
                            (else            0.0))) ; valign/baseline
                  (dx     (vp->dx vp x))
                  (dy     (vp->dy vp y))
                  (angle  (ds-rotation (ds)))
                  (c      (cos angle))
                  (s      (sin angle)))
             (emit-stroke-color! (ds-pen (ds)))
             (in-text-mode (pctx)
               (set-font (pctx)
                         (font-name (font-obj/ensure! pdfname))
                         size)
               ;; Normalize -0.0 to 0.0 so the emitted Tm reads cleanly
               ;; for the common upright case.
               (set-text-matrix (pctx)
                                (if (zero? c) 0.0 c) (if (zero? s) 0.0 s)
                                (if (zero? s) 0.0 (- s)) (if (zero? c) 0.0 c)
                                (- dx (+ (* c tx) (* (- s) ty)))
                                (- dy (+ (* s tx) (* c ty))))
               (draw-text (pctx) (pdf-escape-string clean)))))))

      ;; Text extents
      ;;
      ;; Returns (values width height ascent descent) in device points,
      ;; with descent as a positive magnitude.  Not called during
      ;; normal plot rendering (layout uses heuristics); provided for
      ;; external callers.

      ((backend/text-extents self text)
       (ensure-open!)
       (let* ((clean   (sanitize-text text))
              (size    (ds-font-size (ds)))
              (pdfname (resolve-pdf-font-name (ds-font-family (ds))
                                              (ds-font-slant  (ds))
                                              (ds-font-weight (ds))))
              (asc     (font-ascent/pdf  pdfname size))
              (desc    (font-descent/pdf pdfname size)))
         (values (get-string-width pdfname size clean)
                 (- asc desc)
                 asc
                 (- desc))))

      ;; Dimensions

      ((backend/get-width  self) w*)
      ((backend/get-height self) h*))))

) ;; end module gg-backend-pdf
