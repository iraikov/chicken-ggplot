;; gg-primitives-vge.scm
;; Drawing combinators for the Grammar of Graphics library.
;;
;; ARCHITECTURE
;; ------------
;; Every drawing primitive returns a DRAWER:
;;
;;   drawer = record { proc : (vge → void), bounds : bbox | #f }
;;
;; A drawer's proc, when called with a VGE, emits gfx-instr values
;; into that VGE.  No I/O happens here; all I/O is deferred to the
;; render phase (vge-render! in gg-vge.scm).
;;
;; This makes drawers:
;;   - Pure: no side effects except accumulation into the VGE
;;   - Composable: combine, with-pushed-state, etc. work structurally
;;   - Inspectable: drawer->instructions gives the IR list
;;   - Backend-independent: the same drawer tree works with Cairo,
;;     libplot, SVG, or any future backend
;;
;; STYLE MODEL
;; -----------
;; Style functions (with-pen-color, with-fill-color, etc.) work by
;; emitting a gfx:group containing:
;;   1. A style-setting instruction
;;   2. The inner drawer's instructions
;;
;; Because gfx:group has implicit push/pop semantics at render time,
;; the style change is automatically scoped.  No explicit push/pop
;; instructions appear in the user-facing API.
;;
;; COORDINATE CONVENTION
;; ---------------------
;; Right-handed: origin at bottom-left, Y-up.  All coordinates are in
;; user space; the backend handles the mapping to device pixels.

(module gg-primitives-vge
  ( ;; ── Drawer record ────────────────────────────────────────────────
    make-drawer  drawer?  drawer-proc  drawer-bounds
    empty-drawer

    ;; ── Rendering ────────────────────────────────────────────────────
    ;; Sends a drawer's instructions into a VGE (emit phase).
    render-drawer

    ;; ── Composition ──────────────────────────────────────────────────
    combine         ;; sequential composition (no scoping)
    combine-group   ;; sequential composition inside a gfx:group scope

    ;; ── Scoped state wrappers ────────────────────────────────────────
    ;; All wrappers produce a gfx:group so the scope is explicit in the IR.
    with-pushed-state   ;; raw push/pop scope (low-level)
    with-translate      ;; translate then delegate
    with-viewport       ;; set viewport then delegate
    with-clip-rect      ;; set clip rect then delegate

    ;; ── Style wrappers ───────────────────────────────────────────────
    with-pen-color      ;; (with-pen-color color drawer)
    with-fill-color     ;; (with-fill-color color drawer)
    with-line-width     ;; (with-line-width width drawer)
    with-dash           ;; (with-dash dashes offset drawer)
    with-font           ;; (with-font family size slant weight drawer)
    with-rotate         ;; (with-rotate angle drawer)

    ;; ── Stroked drawing primitives ───────────────────────────────────
    line-drawer         ;; (line-drawer x1 y1 x2 y2)
    polyline-drawer     ;; (polyline-drawer pts)
    rect-drawer         ;; (rect-drawer x y w h)
    circle-drawer       ;; (circle-drawer cx cy r)
    polygon-drawer      ;; (polygon-drawer pts)

    ;; ── Filled drawing primitives ────────────────────────────────────
    filled-rect-drawer
    filled-circle-drawer
    filled-polygon-drawer

    ;; ── General path ─────────────────────────────────────────────────
    path-drawer         ;; (path-drawer cmds #:filled? #f)

    ;; ── Text ─────────────────────────────────────────────────────────
    text-drawer         ;; (text-drawer x y text #:halign #:valign)

    ;; ── Named convenience wrappers ───────────────────────────────────
    ;; Match the interface expected by gg-geom / gg-plot
    h-line-drawer       ;; horizontal line
    v-line-drawer       ;; vertical line
    filled-rect+border-drawer

    ;; ── Bounding box type ────────────────────────────────────────────
    make-bbox  bbox?  bbox-x  bbox-y  bbox-w  bbox-h
    bbox-union  bbox-expand
  )

  (import scheme (chicken base)
          srfi-1
          gg-backend
          gg-vge)

;;; ================================================================
;;; Bounding box record
;;; Used for layout (legend placement, label collision) and optional
;;; viewport optimisation.
;;; ================================================================

(define-record-type <bbox>
  (make-bbox x y w h)
  bbox?
  (x bbox-x) (y bbox-y) (w bbox-w) (h bbox-h))

(define (bbox-union a b)
  (if (not a) b
      (if (not b) a
          (let* ((x0 (min (bbox-x a) (bbox-x b)))
                 (y0 (min (bbox-y a) (bbox-y b)))
                 (x1 (max (+ (bbox-x a) (bbox-w a))
                          (+ (bbox-x b) (bbox-w b))))
                 (y1 (max (+ (bbox-y a) (bbox-h a))
                          (+ (bbox-y b) (bbox-h b)))))
            (make-bbox x0 y0 (- x1 x0) (- y1 y0))))))

;;; Expand a bbox symmetrically by margin on all sides.
(define (bbox-expand bbox margin)
  (make-bbox (- (bbox-x bbox) margin)
             (- (bbox-y bbox) margin)
             (+ (bbox-w bbox) (* 2.0 margin))
             (+ (bbox-h bbox) (* 2.0 margin))))

;;; ================================================================
;;; Drawer record
;;; ================================================================

(define-record-type <drawer>
  (%make-drawer proc bounds)
  drawer?
  (proc   drawer-proc)    ;; (vge → void)
  (bounds drawer-bounds)) ;; <bbox> | #f

(define (make-drawer proc bounds)
  (%make-drawer proc bounds))

;;; A drawer that emits nothing.
(define empty-drawer
  (make-drawer (lambda (vge) (void)) #f))

;;; ================================================================
;;; Rendering
;;; ================================================================
;;;
;;; render-drawer sends a drawer's instructions into a VGE.
;;; This is the emit phase — not the render phase.
;;; The VGE must subsequently be rendered via (vge-render! vge backend).

(define (render-drawer drawer vge)
  (cond
    ((drawer? drawer)    ((drawer-proc drawer) vge))
    ((procedure? drawer) (drawer vge))   ;; backward compat shim
    ((eq? drawer 'empty) (void))
    (else (error "render-drawer: expected a drawer" drawer))))

;;; ================================================================
;;; Composition
;;; ================================================================

;;; Emit all child drawers in order, with NO surrounding scope.
;;; The flat emission is correct for adjacent siblings that share style.
(define (combine . drawers)
  (make-drawer
    (lambda (vge)
      (for-each (lambda (d) (render-drawer d vge)) drawers))
    (fold (lambda (d acc) (bbox-union acc (drawer-bounds d)))
          #f drawers)))

;;; Like combine but wraps the whole body in a gfx:group (push/pop scope).
;;; Useful when the children should be isolated from surrounding style state.
(define (combine-group . drawers)
  (let ((inner (apply combine drawers)))
    (make-drawer
      (lambda (vge)
        (vge-group! vge (lambda (inner-vge) (render-drawer inner inner-vge))))
      (drawer-bounds inner))))

;;; ================================================================
;;; Scoped state wrappers
;;; ================================================================
;;;
;;; All wrappers produce a gfx:group node so the scope is explicit and
;;; visible in the instruction tree.  The optimizer can inspect group
;;; nodes without simulating a push/pop stack.

;;; Wrap a drawer in a push/pop scope with no additional setup.
;;; This is the low-level escape hatch.  Prefer the typed wrappers below.
(define (with-pushed-state drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge (lambda (g) (render-drawer drawer g))))
    (drawer-bounds drawer)))

;;; Translate the coordinate origin by (dx, dy) for the child drawer.
(define (with-translate dx dy drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:translate (exact->inexact dx)
                                       (exact->inexact dy)))
          (render-drawer drawer g))))
    ;; Shift the bounding box if present
    (and (drawer-bounds drawer)
         (let ((b (drawer-bounds drawer)))
           (make-bbox (+ (bbox-x b) dx) (+ (bbox-y b) dy)
                      (bbox-w b) (bbox-h b))))))

;;; Establish a new viewport for the child drawer.
(define (with-viewport x0 y0 x1 y1 drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-viewport (exact->inexact x0)
                                          (exact->inexact y0)
                                          (exact->inexact x1)
                                          (exact->inexact y1)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

;;; Clip to a rectangle for the child drawer.
;;; (x, y) = bottom-left; w, h > 0.
(define (with-clip-rect x y w h drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-clip-rect (exact->inexact x) (exact->inexact y)
                                           (exact->inexact w) (exact->inexact h)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

;;; ================================================================
;;; Style wrappers
;;; ================================================================
;;;
;;; Each sets one style property inside a group scope, then delegates
;;; to the child drawer.  Because the scope is a gfx:group, the style
;;; change is automatically reverted at render time.

(define (with-pen-color color drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-pen-color (parse-color color)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

(define (with-fill-color color drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-fill-color (parse-color color)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

(define (with-line-width width drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-line-width (exact->inexact width)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

(define (with-dash dashes offset drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-dash (map exact->inexact dashes)
                                      (exact->inexact offset)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

(define (with-font family size slant weight drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-font family (exact->inexact size) slant weight))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

;;; Rotate the text anchor for child drawers by angle radians.
;;; Positive angle = counter-clockwise (Y-up convention).
;;; The scope is a gfx:group so rotation is automatically reset on exit.
;;; Only affects text draws; stroked/filled geometry is unaffected.
(define (with-rotate angle drawer)
  (make-drawer
    (lambda (vge)
      (vge-group! vge
        (lambda (g)
          (vge-emit! g (gfx:set-rotation (exact->inexact angle)))
          (render-drawer drawer g))))
    (drawer-bounds drawer)))

;;; Convenience: set pen and fill color together.
(define (with-color pen fill drawer)
  (with-pen-color  pen
    (with-fill-color fill drawer)))

;;; ================================================================
;;; Drawing primitives
;;; ================================================================
;;;
;;; These are the leaf nodes of the drawer tree.  Each emits exactly
;;; one gfx-instr (or a small constant number).  No style instructions
;;; are emitted here. Callers should wrap with the style combinators
;;; above, or set style on the surrounding VGE context.

(define (line-drawer x1 y1 x2 y2)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-line (exact->inexact x1) (exact->inexact y1)
                        (exact->inexact x2) (exact->inexact y2))))
    #f))

(define (polyline-drawer pts)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-polyline
          (map (lambda (p) (cons (exact->inexact (car p))
                                 (exact->inexact (cdr p)))) pts))))
    #f))

;;; (x, y) = bottom-left corner; w, h > 0
(define (rect-drawer x y w h)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-rect (exact->inexact x) (exact->inexact y)
                        (exact->inexact w) (exact->inexact h))))
    (make-bbox x y w h)))

(define (circle-drawer cx cy r)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-circle (exact->inexact cx) (exact->inexact cy)
                          (exact->inexact r))))
    (make-bbox (- cx r) (- cy r) (* 2.0 r) (* 2.0 r))))

(define (polygon-drawer pts)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-polygon
          (map (lambda (p) (cons (exact->inexact (car p))
                                 (exact->inexact (cdr p)))) pts))))
    #f))

(define (filled-rect-drawer x y w h)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-filled-rect (exact->inexact x) (exact->inexact y)
                               (exact->inexact w) (exact->inexact h))))
    (make-bbox x y w h)))

(define (filled-circle-drawer cx cy r)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-filled-circle (exact->inexact cx) (exact->inexact cy)
                                 (exact->inexact r))))
    (make-bbox (- cx r) (- cy r) (* 2.0 r) (* 2.0 r))))

(define (filled-polygon-drawer pts)
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-filled-polygon
          (map (lambda (p) (cons (exact->inexact (car p))
                                 (exact->inexact (cdr p)))) pts))))
    #f))

;;; General path drawer.
;;; cmds    = list of path-cmd values (from gg-backend)
;;; filled? = if #t, fill then stroke; else stroke only
(define (path-drawer cmds #!key (filled? #f))
  (make-drawer
    (lambda (vge)
      (vge-emit! vge (gfx:draw-path cmds filled?)))
    #f))

;;; ================================================================
;;; Text drawer
;;; ================================================================

(define (text-drawer x y text
          #!key
          (halign halign/left)
          (valign valign/baseline))
  (make-drawer
    (lambda (vge)
      (vge-emit! vge
        (gfx:draw-text (exact->inexact x) (exact->inexact y)
                        text halign valign)))
    #f))

;;; ================================================================
;;; Named convenience wrappers
;;; ================================================================
;;;
;;; These correspond to the helpers used throughout gg-geom and gg-plot.

;;; Horizontal line from x1 to x2 at height y.
(define (h-line-drawer x1 x2 y)
  (line-drawer x1 y x2 y))

;;; Vertical line from y1 to y2 at position x.
(define (v-line-drawer x y1 y2)
  (line-drawer x y1 x y2))

;;; Filled rectangle with a distinct border color.
;;; fill-color and pen-color are color values (or parseable strings).
;;; This is the common case for bars, tiles, legend keys.
(define (filled-rect+border-drawer x y w h fill-color pen-color)
  (with-color pen-color fill-color
    (filled-rect-drawer x y w h)))

) ;; end module gg-primitives-vge
