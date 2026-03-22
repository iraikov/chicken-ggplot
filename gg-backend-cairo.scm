;; gg-backend-cairo.scm
;; Cairo graphics backend for the gg Grammar of Graphics library.
;;
;; Exports four backend constructors:
;;   (make-cairo-png-backend "plot.png" 800 600)
;;   (make-cairo-svg-backend "plot.svg" 800 600)
;;   (make-cairo-pdf-backend "plot.pdf" 595 842)  ; A4 in pt
;;   (make-cairo-ps-backend  "plot.ps"  595 842)
;;
;; COORDINATE SYSTEM
;; -----------------
;; The gg protocol uses bottom-left origin with Y-up.
;; Cairo uses top-left origin with Y-down.
;;
;; We do explicit coordinate conversion on every draw call rather than
;; baking a Y-flip into Cairo's CTM.  Reason: with a flipped CTM, all
;; text renders upside-down and font metrics (bearing, advance) flip sign.
;; Explicit conversion keeps Cairo's coordinate space as plain device space
;; (Y-down, origin top-left, units = pixels), and text is always drawn in
;; that clean device space after computing the anchor device-coordinate.
;;
;; Viewport mapping:
;;   device_x = (user_x - x0) / (x1 - x0) * width
;;   device_y = (y1 - user_y) / (y1 - y0) * height   <- Y flipped here
;;
;; STATE STACK
;; -----------
;; Cairo's save/restore manages drawing attributes (color, line width,
;; dash, clip, CTM).  On the Scheme-side draw-state record manages the
;; viewport.  backend/push-state! does both; backend/pop-state! does
;; both and re-applies the saved viewport.

(module gg-backend-cairo
        (make-cairo-png-backend
         make-cairo-svg-backend
         make-cairo-pdf-backend
         make-cairo-ps-backend)

        (import scheme (chicken base)
                srfi-4 ;; f64vector for cairo matrix / dash
                datatype
                yasos
                cairo
                gg-backend)

;;; ================================================================
;;; Viewport record
;;; Stores the user->device mapping for the current drawing context.
;;; ================================================================

(define-record-type <viewport>
  (%make-vp x0 y0 x1 y1 dev-w dev-h)
  viewport?
  (x0    vp-x0)
  (y0    vp-y0)
  (x1    vp-x1)
  (y1    vp-y1)
  (dev-w vp-dev-w)   ;; device width  (pixels/points)
  (dev-h vp-dev-h))  ;; device height (pixels/points)

(define (make-identity-viewport w h)
  (%make-vp 0.0 0.0 w h w h))

;;; User X -> device X
(define (vp->dx vp ux)
  (* (/ (- ux (vp-x0 vp)) (- (vp-x1 vp) (vp-x0 vp))) (vp-dev-w vp)))

;;; User Y -> device Y  (Y-flip: user Y-up becomes device Y-down)
(define (vp->dy vp uy)
  (* (/ (- (vp-y1 vp) uy) (- (vp-y1 vp) (vp-y0 vp))) (vp-dev-h vp)))

;;; Scale a user-space length -> device pixels  (unsigned; uses X axis)
(define (vp-scale-x vp len)
  (* (/ (abs len) (- (vp-x1 vp) (vp-x0 vp))) (vp-dev-w vp)))

;;; Scale a user-space length -> device pixels  (unsigned; uses Y axis)
(define (vp-scale-y vp len)
  (* (/ (abs len) (- (vp-y1 vp) (vp-y0 vp))) (vp-dev-h vp)))

;;; Translate viewport: shift user-space origin by (dx, dy).
;;; Placing the local origin at user-space (dx, dy) means subtracting
;;; the offset from the viewport window boundaries so that local (0,0)
;;; maps to device pixel (dx, H-dy) — i.e. a true affine translate.
(define (vp-translate vp dx dy)
  (%make-vp (- (vp-x0 vp) dx) (- (vp-y0 vp) dy)
            (- (vp-x1 vp) dx) (- (vp-y1 vp) dy)
            (vp-dev-w vp)     (vp-dev-h vp)))

;;; ================================================================
;;; Draw-state record
;;; All fields that backend/push-state! / backend/pop-state! must save.
;;; ================================================================

(define-record-type <draw-state>
  (%make-ds viewport pen fill line-width
            font-family font-size font-slant font-weight)
  draw-state?
  (viewport    ds-viewport    ds-viewport-set!)
  (pen         ds-pen         ds-pen-set!)
  (fill        ds-fill        ds-fill-set!)
  (line-width  ds-line-width  ds-line-width-set!)
  (font-family ds-font-family ds-font-family-set!)
  (font-size   ds-font-size   ds-font-size-set!)
  (font-slant  ds-font-slant  ds-font-slant-set!)
  (font-weight ds-font-weight ds-font-weight-set!))

(define (make-default-draw-state w h)
  (%make-ds (make-identity-viewport w h)
            color-black color-white
            1.0
            "sans-serif" 12.0 'normal 'normal))

(define (copy-draw-state ds)
  (%make-ds (ds-viewport    ds) (ds-pen    ds) (ds-fill ds)
            (ds-line-width  ds)
            (ds-font-family ds) (ds-font-size ds)
            (ds-font-slant  ds) (ds-font-weight ds)))

;;; ================================================================
;;; Cairo helper shims
;;; ================================================================

(define (slant->cairo s)
  (case s
    ((italic)  CAIRO_FONT_SLANT_ITALIC)
    ((oblique) CAIRO_FONT_SLANT_OBLIQUE)
    (else      CAIRO_FONT_SLANT_NORMAL)))

(define (weight->cairo w)
  (case w
    ((bold) CAIRO_FONT_WEIGHT_BOLD)
    (else   CAIRO_FONT_WEIGHT_NORMAL)))

;;; Set Cairo source color from a `color` value.
(define (cairo-set-color! ctx col)
  (call-with-values
    (lambda () (color->rgba-values col))
    (lambda (r g b a) (cairo-set-source-rgba ctx r g b a))))

;;; Apply the font stored in draw-state to ctx.
;;; Must be called in device-coordinate context (identity CTM).
(define (cairo-apply-font! ctx ds)
  (cairo-select-font-face ctx
    (ds-font-family ds)
    (slant->cairo  (ds-font-slant  ds))
    (weight->cairo (ds-font-weight ds)))
  (cairo-set-font-size ctx (ds-font-size ds)))

;;; ================================================================
;;; Path building
;;; Translates a list of path-cmd values into Cairo path calls.
;;; All coordinates are converted from user space to device space via vp.
;;; ================================================================

(define (cairo-build-path! ctx vp cmds)
  (cairo-new-path ctx)
  (for-each
    (lambda (cmd)
      (cases path-cmd cmd
        (path:move-to (x y)
         (cairo-move-to ctx (vp->dx vp x) (vp->dy vp y)))
        (path:line-to (x y)
         (cairo-line-to ctx (vp->dx vp x) (vp->dy vp y)))
        (path:curve-to (x1 y1 x2 y2 x3 y3)
         (cairo-curve-to ctx
           (vp->dx vp x1) (vp->dy vp y1)
           (vp->dx vp x2) (vp->dy vp y2)
           (vp->dx vp x3) (vp->dy vp y3)))
        (path:arc (cx cy r a1 a2)
         ;; The Y-flip reverses arc winding in device space.
         ;; Negate and swap angles to preserve the user-space direction.
         (cairo-arc ctx
           (vp->dx vp cx) (vp->dy vp cy)
           (vp-scale-x vp r)
           (- a2) (- a1)))
        (path:close ()
         (cairo-close-path ctx))))
    cmds))

;;; ================================================================
;;; Text rendering in device space
;;;
;;; In the gg convention, text is specified at a user-space anchor.
;;; We map that anchor to device coordinates, then draw the text
;;; with Cairo's CTM reset to identity so it is upright and at the
;;; correct device-pixel size.
;;;
;;; The inner cairo-save / cairo-restore protects any outer saved
;;; state; we only use cairo-identity-matrix within the inner save.
;;; ================================================================

(define (cairo-draw-text! ctx vp ds x y text halign valign)
  (let ((dx (vp->dx vp x))
        (dy (vp->dy vp y)))
    (cairo-save ctx)
    ;; Apply font in device space (identity CTM)
    (cairo-identity-matrix ctx)
    (cairo-apply-font! ctx ds)
    (cairo-set-color! ctx (ds-pen ds))
    ;; Measure text for alignment adjustment
    (let ((te (make-cairo-text-extents-type)))
      (cairo-text-extents ctx text te)
      (let* ((tw  (cairo-text-extents-width    te))
             (th  (cairo-text-extents-height   te))
             (tb  (cairo-text-extents-y-bearing te))   ; usually negative
             ;; Horizontal anchor
             (ax  (case halign
                    ((halign/center) (- dx (/ tw 2.0)))
                    ((halign/right)  (- dx tw))
                    (else             dx)))              ; halign/left
             ;; Vertical anchor
             ;; Cairo baseline origin: positive y = below baseline
             (ay  (case valign
                    ((valign/center)   (- dy (+ (/ th 2.0) tb)))
                    ((valign/bottom)   (- dy (+ th tb)))
                    ((valign/baseline) dy)
                    (else              (- dy tb)))))     ; valign/top
        (cairo-move-to ctx ax ay)
        (cairo-show-text ctx text)))
    (cairo-restore ctx)))

;;; ================================================================
;;; Backend factory
;;; ================================================================
;;;
;;; make-surface! : thunk -> cairo_surface_t
;;; on-close!     : cairo_surface_t -> void
;;;                 Called (with surface still valid) before disposal.
;;;                 Used by the PNG backend to write the file.

(define (make-cairo-backend* make-surface! width height on-close!)
  (let* ((w*   (exact->inexact width))
         (h*   (exact->inexact height))
         (ctx-cell  (list #f))   ;; mutable cell: #f or cairo_t
         (surf-cell (list #f))   ;; mutable cell: #f or cairo_surface_t
         (ds-cell   (list #f))   ;; mutable cell: #f or <draw-state>
         (stack     (list '())))  ;; mutable cell: list of saved draw-states

    (define (ctx)  (car ctx-cell))
    (define (surf) (car surf-cell))
    (define (ds)   (car ds-cell))

    (define (sync-line-width!)
      (cairo-set-line-width (ctx) (ds-line-width (ds))))

    (object
      ((graphics-backend? self) #t)

      ;; Lifecycle

      ((backend/open! self)
       (let* ((s  (make-surface!))
              (c  (cairo-create s))
              (d  (make-default-draw-state w* h*)))
         (set-car! surf-cell s)
         (set-car! ctx-cell  c)
         (set-car! ds-cell   d)
         (set-car! stack     '())
         (sync-line-width!)))

      ((backend/close! self)
       (when (ctx)
         (cairo-surface-flush (surf))
         (on-close! (surf))
         (cairo-surface-finish (surf))
         (cairo-destroy (ctx))
         (cairo-surface-destroy (surf))
         (set-car! ctx-cell  #f)
         (set-car! surf-cell #f)
         (set-car! ds-cell   #f)))

      ;; State stack
      ;;
      ;; cairo-save saves: source pattern, line width, dash, line cap/join,
      ;; fill rule, font, operator, clip.  It does NOT save our viewport.
      ;; We save/restore that separately via ds-stack.

      ((backend/push-state! self)
       (cairo-save (ctx))
       (set-car! stack (cons (copy-draw-state (ds)) (car stack))))

      ((backend/pop-state! self)
       (when (null? (car stack))
         (error "backend/pop-state!: state stack underflow"))
       (cairo-restore (ctx))
       (set-car! ds-cell (car (car stack)))
       (set-car! stack   (cdr (car stack)))
       ;; Cairo-restore restores line width too, but sync our record
       ;; just in case the restored ds-line-width differs.
       (sync-line-width!))

      ;; Viewport

      ((backend/set-viewport! self x0 y0 x1 y1)
       (ds-viewport-set! (ds)
         (%make-vp (exact->inexact x0) (exact->inexact y0)
                   (exact->inexact x1) (exact->inexact y1)
                   w* h*)))

      ((backend/translate! self dx dy)
       (ds-viewport-set! (ds)
         (vp-translate (ds-viewport (ds))
                       (exact->inexact dx)
                       (exact->inexact dy))))

      ;; Clipping
      ;;
      ;; (x, y) = bottom-left of clip rect in user space.

      ((backend/set-clip-rect! self x y w h)
       (let* ((vp  (ds-viewport (ds)))
              (dx  (vp->dx vp x))
              (dyt (vp->dy vp (+ y h)))   ;; device y of TOP edge
              (dw  (vp-scale-x vp w))
              (dh  (vp-scale-y vp h)))
         (cairo-new-path (ctx))
         (cairo-rectangle (ctx) dx dyt dw dh)
         (cairo-clip (ctx))))

      ((backend/reset-clip! self)
       (cairo-reset-clip (ctx)))

      ;; Style

      ((backend/set-pen-color! self color)
       (ds-pen-set! (ds) (parse-color color)))

      ((backend/set-fill-color! self color)
       (ds-fill-set! (ds) (parse-color color)))

      ((backend/set-line-width! self w)
       (let ((w* (exact->inexact w)))
         (ds-line-width-set! (ds) w*)
         (cairo-set-line-width (ctx) w*)))

      ((backend/set-dash! self dashes offset)
       (let* ((n (length dashes))
              (v (list->f64vector (map exact->inexact dashes))))
         (cairo-set-dash (ctx) v n (exact->inexact offset))))

      ((backend/set-font! self family size slant weight)
       (ds-font-family-set! (ds) family)
       (ds-font-size-set!   (ds) (exact->inexact size))
       (ds-font-slant-set!  (ds) slant)
       (ds-font-weight-set! (ds) weight))

      ;; Stroked primitives

      ((backend/draw-line! self x1 y1 x2 y2)
       (let ((vp (ds-viewport (ds))))
         (cairo-new-path (ctx))
         (cairo-set-color! (ctx) (ds-pen (ds)))
         (cairo-move-to (ctx) (vp->dx vp x1) (vp->dy vp y1))
         (cairo-line-to (ctx) (vp->dx vp x2) (vp->dy vp y2))
         (cairo-stroke (ctx))))

      ((backend/draw-polyline! self pts)
       (when (pair? pts)
         (let ((vp (ds-viewport (ds))))
           (cairo-new-path (ctx))
           (cairo-set-color! (ctx) (ds-pen (ds)))
           (cairo-move-to (ctx)
             (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
           (for-each
             (lambda (pt)
               (cairo-line-to (ctx)
                 (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
             (cdr pts))
           (cairo-stroke (ctx)))))

      ;; (x, y) = bottom-left; Cairo rectangle needs device top-left.
      ((backend/draw-rect! self x y w h)
       (let* ((vp  (ds-viewport (ds)))
              (dx  (vp->dx vp x))
              (dyt (vp->dy vp (+ y h)))
              (dw  (vp-scale-x vp w))
              (dh  (vp-scale-y vp h)))
         (cairo-new-path (ctx))
         (cairo-set-color! (ctx) (ds-pen (ds)))
         (cairo-rectangle (ctx) dx dyt dw dh)
         (cairo-stroke (ctx))))

      ((backend/draw-circle! self cx cy r)
       (let* ((vp (ds-viewport (ds)))
              (dx (vp->dx vp cx))
              (dy (vp->dy vp cy))
              (dr (vp-scale-x vp r)))
         (cairo-new-path (ctx))
         (cairo-set-color! (ctx) (ds-pen (ds)))
         (cairo-arc (ctx) dx dy dr 0.0 (* 2.0 cairo-pi))
         (cairo-stroke (ctx))))

      ((backend/draw-polygon! self pts)
       (when (pair? pts)
         (let ((vp (ds-viewport (ds))))
           (cairo-new-path (ctx))
           (cairo-set-color! (ctx) (ds-pen (ds)))
           (cairo-move-to (ctx)
             (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
           (for-each
             (lambda (pt)
               (cairo-line-to (ctx)
                 (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
             (cdr pts))
           (cairo-close-path (ctx))
           (cairo-stroke (ctx)))))

      ;; Filled primitives (fill-preserve then stroke)

      ((backend/draw-filled-rect! self x y w h)
       (let* ((vp  (ds-viewport (ds)))
              (dx  (vp->dx vp x))
              (dyt (vp->dy vp (+ y h)))
              (dw  (vp-scale-x vp w))
              (dh  (vp-scale-y vp h)))
         (cairo-new-path (ctx))
         (cairo-rectangle (ctx) dx dyt dw dh)
         (cairo-set-color! (ctx) (ds-fill (ds)))
         (cairo-fill-preserve (ctx))
         (cairo-set-color! (ctx) (ds-pen (ds)))
         (cairo-stroke (ctx))))

      ((backend/draw-filled-circle! self cx cy r)
       (let* ((vp (ds-viewport (ds)))
              (dx (vp->dx vp cx))
              (dy (vp->dy vp cy))
              (dr (vp-scale-x vp r)))
         (cairo-new-path (ctx))
         (cairo-arc (ctx) dx dy dr 0.0 (* 2.0 cairo-pi))
         (cairo-set-color! (ctx) (ds-fill (ds)))
         (cairo-fill-preserve (ctx))
         (cairo-set-color! (ctx) (ds-pen (ds)))
         (cairo-stroke (ctx))))

      ((backend/draw-filled-polygon! self pts)
       (when (pair? pts)
         (let ((vp (ds-viewport (ds))))
           (cairo-new-path (ctx))
           (cairo-move-to (ctx)
             (vp->dx vp (caar pts)) (vp->dy vp (cdar pts)))
           (for-each
             (lambda (pt)
               (cairo-line-to (ctx)
                 (vp->dx vp (car pt)) (vp->dy vp (cdr pt))))
             (cdr pts))
           (cairo-close-path (ctx))
           (cairo-set-color! (ctx) (ds-fill (ds)))
           (cairo-fill-preserve (ctx))
           (cairo-set-color! (ctx) (ds-pen (ds)))
           (cairo-stroke (ctx)))))

      ;; General path

      ((backend/draw-path! self cmds filled?)
       (cairo-build-path! (ctx) (ds-viewport (ds)) cmds)
       (if filled?
           (begin
             (cairo-set-color! (ctx) (ds-fill (ds)))
             (cairo-fill-preserve (ctx))
             (cairo-set-color! (ctx) (ds-pen (ds)))
             (cairo-stroke (ctx)))
           (begin
             (cairo-set-color! (ctx) (ds-pen (ds)))
             (cairo-stroke (ctx)))))

      ;; Text

      ((backend/draw-text! self x y text halign valign)
       (cairo-draw-text! (ctx) (ds-viewport (ds)) (ds) x y text halign valign))

      ;; Returns (values width height ascent descent) in device pixels.
      ((backend/text-extents self text)
       (cairo-save (ctx))
       (cairo-identity-matrix (ctx))
       (cairo-apply-font! (ctx) (ds))
       (let ((te (make-cairo-text-extents-type))
             (fe (make-cairo-font-extents-type)))
         (cairo-text-extents (ctx) text te)
         (cairo-font-extents (ctx) fe)
         (let ((w   (cairo-text-extents-width  te))
               (h   (cairo-text-extents-height te))
               (asc (cairo-font-extents-ascent  fe))
               (dsc (cairo-font-extents-descent fe)))
           (cairo-restore (ctx))
           (values w h asc dsc))))

      ;; Dimensions

      ((backend/get-width  self) w*)
      ((backend/get-height self) h*))))

;;; ================================================================
;;; Public backend constructors
;;; ================================================================

;;; SVG, PDF, PS: surface writes to file during drawing; on-close! is a no-op.
(define (make-cairo-svg-backend filename width height)
  (make-cairo-backend*
    (lambda ()
      (cairo-svg-surface-create filename
        (exact->inexact width) (exact->inexact height)))
    width height
    (lambda (surf) (void))))

(define (make-cairo-pdf-backend filename width height)
  (make-cairo-backend*
    (lambda ()
      (cairo-pdf-surface-create filename
        (exact->inexact width) (exact->inexact height)))
    width height
    (lambda (surf) (void))))

(define (make-cairo-ps-backend filename width height)
  (make-cairo-backend*
    (lambda ()
      (cairo-ps-surface-create filename
        (exact->inexact width) (exact->inexact height)))
    width height
    (lambda (surf) (void))))

;;; PNG: renders to in-memory ARGB32 bitmap; on-close! writes the file.
(define (make-cairo-png-backend filename width height)
  (make-cairo-backend*
    (lambda ()
      (cairo-image-surface-create CAIRO_FORMAT_ARGB32
        (inexact->exact (round width))
        (inexact->exact (round height))))
    width height
    (lambda (surf) (cairo-surface-write-to-png surf filename))))

) ;; end module gg-backend-cairo
