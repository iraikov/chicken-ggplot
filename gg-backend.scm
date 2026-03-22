;; gg-backend.scm
;; Abstract graphics backend protocol for the gg Grammar of Graphics library.
;;
;; This module defines the YASOS interface that all graphics backends must
;; implement.  The two concrete backends are:
;;   gg-backend-cairo.scm   - Cairo (default)
;;   gg-backend-libplot.scm - GNU libplot wrapper
;;
;; COORDINATE CONVENTION
;; ---------------------
;; All drawing operations use a right-handed coordinate system:
;;   origin at BOTTOM-LEFT, X increases RIGHT, Y increases UPWARD.
;; This matches libplot's fspace convention and mathematical tradition.
;; Backend implementations map this to their native device space internally.
;;
;; COLOR CONVENTION
;; ----------------
;; Colors are `color` algebraic datatype values (color:rgba or color:named).
;; Use (parse-color "steelblue"), (parse-color "#4682b4"), or
;; (color:rgba 0.27 0.51 0.71 1.0) directly.

(module gg-backend
  ( ;; Predicate
    graphics-backend?
    ;; Lifecycle
    backend/open!  backend/close!
    ;; State stack - push/pop for nested panel/facet rendering
    backend/push-state!  backend/pop-state!
    ;; Convenience wrapper
    backend/with-state
    ;; Viewport: maps user coordinate rectangle to device surface
    backend/set-viewport!
    ;; Local translation accumulated into the current viewport
    backend/translate!
    ;; Clipping
    backend/set-clip-rect!  backend/reset-clip!
    ;; Pen (stroke) style
    backend/set-pen-color!  backend/set-line-width!  backend/set-dash!
    ;; Fill style
    backend/set-fill-color!
    ;; Text / font  (size in device points, not user coordinates)
    backend/set-font!
    ;; Stroked primitives
    backend/draw-line!
    backend/draw-polyline!   ; list of (x . y) pairs
    backend/draw-rect!       ; (x y) = bottom-left, w h > 0
    backend/draw-circle!
    backend/draw-polygon!    ; list of (x . y) pairs
    ;; Filled primitives  (fills with fill color, strokes with pen color)
    backend/draw-filled-rect!
    backend/draw-filled-circle!
    backend/draw-filled-polygon!
    ;; General path  (list of path-cmd values)
    backend/draw-path!
    ;; Text
    backend/draw-text!
    backend/text-extents     ; (values width height ascent descent)
    ;; Dimension queries
    backend/get-width  backend/get-height
    ;; Color type
    color:rgba  color:named
    color?  color-rgba?  color-named?
    color-r  color-g  color-b  color-a  color-name
    parse-color  color->rgba-values
    color-black  color-white  color-transparent
    ;; Path command type
    path:move-to  path:line-to  path:curve-to  path:arc  path:close
    path-cmd path-cmd?
    ;; Text alignment tokens
    halign/left  halign/center  halign/right
    valign/top   valign/center  valign/bottom  valign/baseline)

  (import scheme
          (chicken base)
          (chicken string)
          yasos
          datatype)

;;; ================================================================
;;; Color type
;;; ================================================================

(define-datatype color color?
  (color:rgba (r real?) (g real?) (b real?) (a real?))
  (color:named (name string?)))

(define (color-rgba? c)
  (cases color c
         (color:rgba (_ _ _ _) #t)
         (else #f)))
(define (color-named? c)
  (cases color c
         (color:named (_) #t)
         (else #f)))

(define (color-r c)
  (cases color c (color:rgba (r _ _ _) r)
    (else (error "color-r: not an rgba color" c))))
(define (color-g c)
  (cases color c (color:rgba (_ g _ _) g)
    (else (error "color-g: not an rgba color" c))))
(define (color-b c)
  (cases color c (color:rgba (_ _ b _) b)
    (else (error "color-b: not an rgba color" c))))
(define (color-a c)
  (cases color c (color:rgba (_ _ _ a) a)
    (else (error "color-a: not an rgba color" c))))
(define (color-name c)
  (cases color c (color:named (n) n)
    (else (error "color-name: not a named color" c))))

;;; Common constants
(define color-black       (color:rgba 0.0 0.0 0.0 1.0))
(define color-white       (color:rgba 1.0 1.0 1.0 1.0))
(define color-transparent (color:rgba 0.0 0.0 0.0 0.0))

;;; Partial X11 / CSS named color table - RGBA components in [0, 1]
(define *color-table*
  '(("none"         0.000 0.000 0.000 0.0)   ; transparent / no color
    ("transparent"  0.000 0.000 0.000 0.0)
    ("black"        0.000 0.000 0.000 1.0)
    ("white"        1.000 1.000 1.000 1.0)
    ("red"          1.000 0.000 0.000 1.0)
    ("green"        0.000 0.502 0.000 1.0)
    ("blue"         0.000 0.000 1.000 1.0)
    ("yellow"       1.000 1.000 0.000 1.0)
    ("orange"       1.000 0.647 0.000 1.0)
    ("purple"       0.502 0.000 0.502 1.0)
    ("cyan"         0.000 1.000 1.000 1.0)
    ("magenta"      1.000 0.000 1.000 1.0)
    ("gray"         0.502 0.502 0.502 1.0)
    ("grey"         0.502 0.502 0.502 1.0)
    ("gray10"       0.100 0.100 0.100 1.0)
    ("gray20"       0.200 0.200 0.200 1.0)
    ("gray30"       0.300 0.300 0.300 1.0)
    ("gray40"       0.400 0.400 0.400 1.0)
    ("gray50"       0.500 0.500 0.500 1.0)
    ("gray60"       0.600 0.600 0.600 1.0)
    ("gray70"       0.700 0.700 0.700 1.0)
    ("gray80"       0.800 0.800 0.800 1.0)
    ("gray85"       0.850 0.850 0.850 1.0)
    ("gray90"       0.900 0.900 0.900 1.0)
    ("gray95"       0.950 0.950 0.950 1.0)
    ("steelblue"    0.275 0.510 0.706 1.0)
    ("coral"        1.000 0.498 0.314 1.0)
    ("salmon"       0.980 0.502 0.447 1.0)
    ("tomato"       1.000 0.388 0.278 1.0)
    ("forestgreen"  0.133 0.545 0.133 1.0)
    ("darkgreen"    0.000 0.392 0.000 1.0)
    ("navy"         0.000 0.000 0.502 1.0)
    ("brown"        0.647 0.165 0.165 1.0)
    ("pink"         1.000 0.753 0.796 1.0)
    ("lightblue"    0.678 0.847 0.902 1.0)
    ("lightgreen"   0.565 0.933 0.565 1.0)
    ("lightyellow"  1.000 1.000 0.878 1.0)
    ("lightgray"    0.827 0.827 0.827 1.0)
    ("lightgrey"    0.827 0.827 0.827 1.0)
    ("darkblue"     0.000 0.000 0.545 1.0)
    ("darkred"      0.545 0.000 0.000 1.0)
    ("whitesmoke"   0.961 0.961 0.961 1.0)
    ("transparent"  0.000 0.000 0.000 0.0)))

;;; Parse a string or existing color value to a `color`.
;;; Accepts: color values, "#rrggbb", "#rrggbbaa", or X11 names.
(define (parse-color x)
  (cond
    ((color? x) x)
    ((string? x)
     (cond
       ;; #rrggbb
       ((and (= (string-length x) 7) (char=? (string-ref x 0) #\#))
        (let* ((r (string->number (substring x 1 3) 16))
               (g (string->number (substring x 3 5) 16))
               (b (string->number (substring x 5 7) 16)))
          (color:rgba (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0)))
       ;; #rrggbbaa
       ((and (= (string-length x) 9) (char=? (string-ref x 0) #\#))
        (let* ((r (string->number (substring x 1 3) 16))
               (g (string->number (substring x 3 5) 16))
               (b (string->number (substring x 5 7) 16))
               (a (string->number (substring x 7 9) 16)))
          (color:rgba (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0))))
       (else (color:named x))))
    (else (error "parse-color: cannot parse" x))))

;;; Extract (values r g b a) from any color-like value.
;;; Named colors are resolved via the table.
(define (color->rgba-values c)
  (cases color (parse-color c)
    (color:rgba (r g b a) (values r g b a))
    (color:named (name)
     (let ((e (assoc name *color-table*)))
       (if e
           (apply values (cdr e))
           (error "color->rgba-values: unknown color name" name))))))

;;; ================================================================
;;; Path command type
;;; ================================================================
;;;
;;; A path is a list of path-cmd values passed to backend/draw-path!.
;;; This mirrors the SVG / Cairo / PostScript path model.

(define-datatype path-cmd path-cmd?
  (path:move-to (x real?) (y real?))
  (path:line-to (x real?) (y real?))
  (path:curve-to
    (x1 real?) (y1 real?)   ; first control point
    (x2 real?) (y2 real?)   ; second control point
    (x3 real?) (y3 real?))  ; end point
  (path:arc
    (cx real?) (cy real?)   ; center
    (r  real?)              ; radius
    (a1 real?) (a2 real?))  ; start/end angles in radians (CCW, Y-up)
  (path:close))

;;; ================================================================
;;; Text alignment tokens
;;; ================================================================

(define halign/left   'halign/left)
(define halign/center 'halign/center)
(define halign/right  'halign/right)

(define valign/top      'valign/top)
(define valign/center   'valign/center)
(define valign/bottom   'valign/bottom)
(define valign/baseline 'valign/baseline)

;;; ================================================================
;;; YASOS backend protocol
;;; ================================================================

(define-predicate graphics-backend?)

;;; Lifecycle

;;; Open the backend: create the surface, allocate resources.
;;; Must be called before any drawing operation.
(define-operation (backend/open! self)
  (error "backend/open!: not implemented" self))

;;; Close the backend: flush, finalize, and free resources.
;;; For PNG backends this writes the file.
(define-operation (backend/close! self)
  (error "backend/close!: not implemented" self))

;;; State stack
;;;
;;; Saves the current viewport, all drawing properties (pen, fill,
;;; line width, font), and any backend-native state (Cairo save,
;;; libplot savestate).  Used for nested panel and facet rendering.

(define-operation (backend/push-state! self)
  (error "backend/push-state!: not implemented" self))

(define-operation (backend/pop-state! self)
  (error "backend/pop-state!: not implemented" self))

;;; Convenience: run a thunk inside a push/pop pair.
(define (backend/with-state b thunk)
  (backend/push-state! b)
  (let ((result (thunk)))
    (backend/pop-state! b)
    result))

;;; Viewport
;;;
;;; Establishes a user coordinate space mapping.  Maps to libplot's
;;; (fspace plotter x0 y0 x1 y1):
;;;
;;;   (x0, y0) = bottom-left corner in user coordinates
;;;   (x1, y1) = top-right  corner in user coordinates
;;;
;;; The backend maps this rectangle to the full device surface.
;;; The push/pop state stack also saves and restores the viewport.

(define-operation (backend/set-viewport! self x0 y0 x1 y1)
  (error "backend/set-viewport!: not implemented" self))

;;; Shift the user-space origin by (dx, dy).  Equivalent to updating
;;; x0 += dx, x1 += dx, y0 += dy, y1 += dy in the current viewport.
;;; Replaces libplot-based `with-translate` in gg-primitives.
(define-operation (backend/translate! self dx dy)
  (error "backend/translate!: not implemented" self))

;;; Clipping
;;;
;;; (x, y) is the bottom-left corner in user space.

(define-operation (backend/set-clip-rect! self x y w h)
  (error "backend/set-clip-rect!: not implemented" self))

(define-operation (backend/reset-clip! self)
  (error "backend/reset-clip!: not implemented" self))

;;; Style

;;; Pen (stroke) color.  Accepts a color value or anything parse-color accepts.
(define-operation (backend/set-pen-color! self color)
  (error "backend/set-pen-color!: not implemented" self))

;;; Line width in device pixels/points (not user coordinates).
(define-operation (backend/set-line-width! self width)
  (error "backend/set-line-width!: not implemented" self))

;;; Dash pattern.  dashes = list of alternating on/off lengths in device pixels.
;;; offset = phase offset.  Pass (dashes '() offset 0.0) to reset to solid.
(define-operation (backend/set-dash! self dashes offset)
  (error "backend/set-dash!: not implemented" self))

;;; Fill color.
(define-operation (backend/set-fill-color! self color)
  (error "backend/set-fill-color!: not implemented" self))

;;; Font selection.
;;;   family  - string, e.g. "sans-serif", "serif", "monospace"
;;;   size    - device points (not user coordinates)
;;;   slant   - symbol: 'normal | 'italic | 'oblique
;;;   weight  - symbol: 'normal | 'bold
(define-operation (backend/set-font! self family size slant weight)
  (error "backend/set-font!: not implemented" self))

;;; Stroked primitives
;;;
;;; All coordinates in current user / viewport space.

(define-operation (backend/draw-line! self x1 y1 x2 y2)
  (error "backend/draw-line!: not implemented" self))

;;; pts - list of (x . y) pairs
(define-operation (backend/draw-polyline! self pts)
  (error "backend/draw-polyline!: not implemented" self))

;;; (x, y) = bottom-left corner; w, h > 0
(define-operation (backend/draw-rect! self x y w h)
  (error "backend/draw-rect!: not implemented" self))

;;; Radius in user coordinates (X scale assumed uniform).
(define-operation (backend/draw-circle! self cx cy r)
  (error "backend/draw-circle!: not implemented" self))

;;; pts - list of (x . y) pairs; path is implicitly closed.
(define-operation (backend/draw-polygon! self pts)
  (error "backend/draw-polygon!: not implemented" self))

;;; Filled primitives
;;;
;;; Each operation fills with the current fill color, then strokes
;;; the outline with the current pen color.  Set pen color to
;;; color-transparent to suppress the stroke.

(define-operation (backend/draw-filled-rect! self x y w h)
  (error "backend/draw-filled-rect!: not implemented" self))

(define-operation (backend/draw-filled-circle! self cx cy r)
  (error "backend/draw-filled-circle!: not implemented" self))

(define-operation (backend/draw-filled-polygon! self pts)
  (error "backend/draw-filled-polygon!: not implemented" self))

;;; General path
;;;
;;; cmds   - list of path-cmd values
;;; filled? - if #t, fill with fill color then stroke; else stroke only

(define-operation (backend/draw-path! self cmds filled?)
  (error "backend/draw-path!: not implemented" self))

;;; Text
;;;
;;; (x, y) is the anchor point in user space.
;;; halign : {halign/left halign/center halign/right}
;;; valign : {valign/top valign/center valign/bottom valign/baseline}

(define-operation (backend/draw-text! self x y text halign valign)
  (error "backend/draw-text!: not implemented" self))

;;; Returns (values width height ascent descent) in device pixels.
(define-operation (backend/text-extents self text)
  (error "backend/text-extents: not implemented" self))

;;; Dimensions

(define-operation (backend/get-width self)
  (error "backend/get-width: not implemented" self))

(define-operation (backend/get-height self)
  (error "backend/get-height: not implemented" self))

) ;; end module gg-backend
