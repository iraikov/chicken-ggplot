;;;; gg-primitives.scm
;;;; Grammar of Graphics - Primitive Drawing Combinators
;;;;
;;;; Functional wrapper around the libplot library.
;;;; Provides composable drawing operations as first-class values.

(module gg-primitives
  (;; Plotter creation
   make-png-plotter
   make-svg-plotter
   make-ps-plotter
   make-x-plotter
   plotter-context?
   plotter-context-plotter
   plotter-context-width
   plotter-context-height
   
   ;; Core drawer abstraction
   drawer?
   render
   combine
   empty-drawer
   render-drawer
   
   ;; Style combinators
   with-pen-color
   with-fill-color
   with-line-width
   with-line-style
   with-font-name
   with-font-size
   with-alpha
   
   ;; Transform combinators
   with-transform
   with-translate
   with-scale
   with-rotate
   
   ;; Primitive shapes
   point
   line
   polyline
   polygon
   rectangle
   circle
   ellipse
   arc
   bezier
   
   ;; Text rendering
   text
   text-centered
   text-box
   
   ;; Utility
   move-to
   clip-rect
   save-restore)

  (import scheme
          (chicken base)
          (chicken format)
          (chicken port)
          (only plot
                PNG SVG X PS DISPLAY
                BITMAPSIZE PAGESIZE
                INTERLACE PAGESIZE X_AUTO_FLUSH META_PORTABLE
                make-plotter delete-plotter
                openpl closepl
                savestate restorestate
                fpoint fconcat fline fmove fcont fcircle fellipse
                farc fbox fbezier3 fspace
                box endpath
                pencolorname fillcolorname
                linewidth flinewidth linemod filltype 
                fontname fontsize ffontsize textangle
                alabel
                Solid Dotted Dotdashed Shortdashed Longdashed
                Left HCenter Right
                Bottom Baseline VCenter Cap-line Top
                )
          srfi-1
          srfi-9
          matchable)

  ;;; ========================================================================
  ;;; Drawer Abstraction
  ;;; ========================================================================
  ;;;
  ;;; A drawer is a procedure (plotter -> void) that encapsulates drawing
  ;;; operations. Drawers are first-class values that can be composed,
  ;;; transformed, and stored.

  (define-record-type drawer-record
    (make-drawer-internal proc)
    drawer?
    (proc drawer-proc))

  (define (make-drawer proc)
    "Create a drawer from a procedure (plotter -> void)"
    (make-drawer-internal proc))

  (define empty-drawer ;; Drawer that does nothing
    (make-drawer (lambda (p) (void))))

  (define (combine . drawers)
    "Sequence multiple drawers, executing left to right"
    (make-drawer
     (lambda (plotter)
       (for-each (lambda (d)
                   (when (drawer? d)
                     ((drawer-proc d) plotter)))
                 drawers))))

  
  (define-record-type plotter-context
    (make-plotter-context plotter width height)
    plotter-context?
    (plotter plotter-context-plotter)
    (width plotter-context-width)
    (height plotter-context-height))

  

  ;;; ========================================================================
  ;;; Plotter Creation
  ;;; ========================================================================

  (define (make-png-plotter filename width height  #!optional (scale 2))
    "Create PNG plotter with specified dimensions"
    (let ((port (open-output-file filename))
          (pixel-width (* width scale))
          (pixel-height (* height scale)))
      (make-plotter-context
       (make-plotter
        (PNG)
        port
        `(,(INTERLACE #t)
          ,(X_AUTO_FLUSH #f) ,(META_PORTABLE #t)
          ,(PAGESIZE (format "~a,~a" width height))
          ,(BITMAPSIZE (format "~ax~a" pixel-width pixel-height))))
       width
       height
       )))

  (define (make-svg-plotter filename width height)
    "Create SVG plotter with specified dimensions"
    (let ((port (open-output-file filename)))
      (make-plotter-context
       (make-plotter
        (SVG)
        port
        (list (PAGESIZE (format "~a,~a" width height))))
       width
       height)))

  (define (make-ps-plotter filename width height)
    "Create PostScript plotter"
    (let ((port (open-output-file filename)))
      (make-plotter-context
       (make-plotter
        (PS)
        port
        (list (PAGESIZE (format "~a,~a" width height))))
       width
       height)))

  (define (make-x-plotter width height #!optional (display-name ":0"))
    "Create X11 plotter for interactive display"
    (make-plotter-context
     (make-plotter
      (X)
      #f
      (list (BITMAPSIZE (format "~ax~a" width height))
            (DISPLAY display-name)))
     width
     height))

  ;;; ========================================================================
  ;;; Rendering
  ;;; ========================================================================

  (define (render drawer plotter-ctx)
  "Execute a drawer on a plotter context, managing openpl/closepl and coordinate system"
  (let ((plotter (plotter-context-plotter plotter-ctx))
        (width (plotter-context-width plotter-ctx))
        (height (plotter-context-height plotter-ctx)))
    (openpl plotter)
    (fspace plotter 0.0 0.0 (exact->inexact width) (exact->inexact height))
    ((drawer-proc drawer) plotter)
    (closepl plotter)
    (delete-plotter plotter)))

  (define (render-drawer drawer plotter)
  "Render a drawer object to an already-open plotter.
   
   This is used for nested rendering within the plot pipeline
   where the plotter is already initialized.
   
   drawer: A drawer record
   plotter: An open plotter object
   
   Returns: void (side-effect: draws to plotter)"
  
  (cond

   ;; Handle drawer record
   ((drawer? drawer)
    ((drawer-proc drawer) plotter))
   
   ;; Handle procedure directly
   ((procedure? drawer)
    (drawer plotter))
   
   ;; Handle empty drawer
   ((eq? drawer 'empty)
    (void))
   
   ;; Error for invalid input
   (else
    (error "render-drawer: expected drawer record or procedure" drawer))))

  
  ;;; ========================================================================
  ;;; Style Combinators
  ;;; ========================================================================
  ;;;
  ;;; Style combinators wrap a drawer with state changes. They use
  ;;; savestate/restorestate to ensure proper cleanup.

  (define (with-pen-color color drawer)
    "Set pen color for drawing operations"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (pencolorname plotter color)
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-fill-color color drawer)
    "Set fill color for filled shapes"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (fillcolorname plotter color)
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-line-width width drawer)
    "Set line width in user coordinates"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (flinewidth plotter (exact->inexact width))
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-line-style style drawer)
    "Set line style: 'solid, 'dotted, 'dashed, etc."
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (linemod plotter 
                (case style
                  ((solid) (Solid))
                  ((dotted) (Dotted))
                  ((dashed) (Shortdashed))
                  ((dot-dashed) (Dotdashed))
                  ((long-dashed) (Longdashed))
                  (else 1)))
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-font-name font drawer)
    "Set font name (e.g., 'HersheySerif)"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (fontname plotter font)
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-font-size size drawer)
    "Set font size in user coordinates"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (ffontsize plotter size)
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-alpha alpha drawer)
    "Set transparency (0.0 = transparent, 1.0 = opaque)"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (filltype plotter 1) ; Enable filling
       (fillcolorname plotter 
                      (format "rgba(~a,~a,~a,~a)" 0 0 0 alpha))
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  ;;; ========================================================================
  ;;; Transform Combinators
  ;;; ========================================================================

  (define (with-transform drawer affine-matrix)
    "Apply arbitrary affine transformation matrix"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       (match affine-matrix
         ((m00 m01 m10 m11 tx ty)
          (fconcat plotter
                   (exact->inexact m00)
                   (exact->inexact m01)
                   (exact->inexact m10)
                   (exact->inexact m11)
                   (exact->inexact tx)
                   (exact->inexact ty))))
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (with-translate dx dy drawer)
    "Translate coordinate system"
    (with-transform drawer (list 1 0 0 1 dx dy)))

  (define (with-scale sx sy drawer)
    "Scale coordinate system"
    (with-transform drawer (list sx 0 0 sy 0 0)))

  (define (with-rotate angle drawer)
    "Rotate coordinate system by angle in radians"
    (let ((cos-a (cos angle))
          (sin-a (sin angle)))
      (with-transform drawer (list cos-a sin-a (- sin-a) cos-a 0 0))))

  ;;; ========================================================================
  ;;; Primitive Shapes
  ;;; ========================================================================

  (define (point x y #!key (color #f) (size 1))
    "Draw a point at (x, y)"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (fpoint plotter (exact->inexact x) (exact->inexact y))))))
      (if color
          (with-pen-color color base)
          base)))

  (define (line x1 y1 x2 y2 #!key (color #f) (width #f) (style 'solid))
    "Draw a line from (x1, y1) to (x2, y2)"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (fline plotter
                          (exact->inexact x1)
                          (exact->inexact y1)
                          (exact->inexact x2)
                          (exact->inexact y2))))))
      (let ((styled (if width
                        (with-line-width width base)
                        base)))
        (let ((styled2 (with-line-style style styled)))
          (if color
              (with-pen-color color styled2)
              styled2)))))

  (define (polyline points #!key (color #f) (width #f) (style 'solid))
    "Draw connected line segments through list of (x . y) points"
    (if (< (length points) 2)
        empty-drawer
        (let ((base (make-drawer
                     (lambda (plotter)
                       (match points
                         (((x . y) . rest)
                          (fmove plotter (exact->inexact x) (exact->inexact y))
                          (for-each
                           (lambda (pt)
                             (fcont plotter (exact->inexact (car pt))
                                    (exact->inexact (cdr pt))))
                           rest)))))))
          (let ((styled (if width
                            (with-line-width width base)
                            base)))
            (let ((styled2 (with-line-style style styled)))
              (if color
                  (with-pen-color color styled2)
                  styled2))))))

  (define (polygon points #!key (fill-color #f) (edge-color #f) (width #f))
    "Draw a filled polygon with optional edge"
    (if (< (length points) 3)
        empty-drawer
        (combine
         ;; Fill if requested
         (if fill-color
             (with-fill-color fill-color
               (make-drawer
                (lambda (plotter)
                  (filltype plotter 1)
                  (match points
                    (((x . y) . rest)
                     (fmove plotter (exact->inexact x) (exact->inexact y))
                     (for-each
                      (lambda (pt)
                        (fcont plotter (exact->inexact (car pt)) (exact->inexact (cdr pt))))
                      rest)
                     (endpath plotter))))))
             empty-drawer)
         ;; Edge if requested
         (if edge-color
             (polyline (append points (list (car points)))
                       #:color edge-color
                       #:width width)
             empty-drawer))))

  (define (rectangle x y width height 
                      #!key (fill-color #f) (edge-color #f) (line-width #f))
    "Draw a rectangle with bottom-left at (x, y)"
    (combine
     ;; Fill
     (if fill-color
         (with-fill-color fill-color
           (make-drawer
            (lambda (plotter)
              (filltype plotter 1)
              (fbox plotter (exact->inexact x) (exact->inexact y)
                    (exact->inexact (+ x width))
                    (exact->inexact (+ y height))))))
         empty-drawer)
     ;; Edge
     (if edge-color
         (let ((base (make-drawer
                      (lambda (plotter)
                        (fbox plotter
                              (exact->inexact x)
                              (exact->inexact y)
                              (exact->inexact (+ x width))
                              (exact->inexact (+ y height)))))))
           (let ((styled (if line-width
                             (with-line-width line-width base)
                             base)))
             (with-pen-color edge-color styled)))
         empty-drawer)))

  (define (circle cx cy radius 
                  #!key (fill-color #f) (edge-color #f) (line-width #f))
    "Draw a circle centered at (cx, cy)"
    (combine
     ;; Fill
     (if fill-color
         (with-fill-color fill-color
           (make-drawer
            (lambda (plotter)
              (filltype plotter 1)
              (fcircle plotter (exact->inexact cx)
                       (exact->inexact cy)
                       (exact->inexact radius)))))
         empty-drawer)
     ;; Edge
     (if edge-color
         (let ((base (make-drawer
                      (lambda (plotter)
                        (circle plotter cx cy radius)))))
           (let ((styled (if line-width
                             (with-line-width line-width base)
                             base)))
             (with-pen-color edge-color styled)))
         empty-drawer)))

  (define (ellipse cx cy rx ry angle
                   #!key (fill-color #f) (edge-color #f) (line-width #f))
    "Draw an ellipse centered at (cx, cy) with radii rx, ry, rotated by angle"
    (combine
     ;; Fill
     (if fill-color
         (with-fill-color fill-color
           (make-drawer
            (lambda (plotter)
              (filltype plotter 1)
              (fellipse plotter cx cy rx ry angle))))
         empty-drawer)
     ;; Edge
     (if edge-color
         (let ((base (make-drawer
                      (lambda (plotter)
                        (ellipse plotter cx cy rx ry angle)))))
           (let ((styled (if line-width
                             (with-line-width line-width base)
                             base)))
             (with-pen-color edge-color styled)))
         empty-drawer)))

  (define (arc cx cy rx ry angle1 angle2
               #!key (color #f) (width #f))
    "Draw an arc from angle1 to angle2 (in degrees)"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (farc plotter cx cy rx ry angle1 angle2)))))
      (let ((styled (if width
                        (with-line-width width base)
                        base)))
        (if color
            (with-pen-color color styled)
            styled))))

  (define (bezier x0 y0 x1 y1 x2 y2 x3 y3
                  #!key (color #f) (width #f))
    "Draw a cubic Bezier curve with control points"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (fbezier3 plotter x0 y0 x1 y1 x2 y2 x3 y3)))))
      (let ((styled (if width
                        (with-line-width width base)
                        base)))
        (if color
            (with-pen-color color styled)
            styled))))

  ;;; ========================================================================
  ;;; Text Rendering
  ;;; ========================================================================

  (define (text x y str #!key (color #f) (font #f) (size #f) (angle 0) (hjust 'center))
    "Draw text at (x, y).
     hjust controls which direction text extends from the cursor (libplot convention):
       'left   - text extends leftward  (right edge at cursor); libplot (Left)
       'right  - text extends rightward (left edge at cursor);  libplot (Right)
       'center - text centered at cursor (default);             libplot (HCenter)"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (savestate plotter)
                   (when (not (zero? angle))
                     (textangle plotter angle))
                   (fmove plotter (exact->inexact x) (exact->inexact y))
                   (alabel plotter
                           (case hjust
                             ((left)  (Left))
                             ((right) (Right))
                             (else    (HCenter)))
                           (VCenter) str)
                   (restorestate plotter)))))
      (let ((with-font (if font
                           (with-font-name font base)
                           base)))
        (let ((with-size (if size
                             (with-font-size size with-font)
                             with-font)))
          (if color
              (with-pen-color color with-size)
              with-size)))))

  (define (text-centered x y str #!key (color #f) (font #f) (size #f))
    "Draw text centered at (x, y)"
    (let ((base (make-drawer
                 (lambda (plotter)
                   (fmove plotter (exact->inexact x) (exact->inexact y))
                   (alabel plotter (HCenter) (VCenter) str)))))
      (let ((with-font (if font
                           (with-font-name font base)
                           base)))
        (let ((with-size (if size
                             (with-font-size size with-font)
                             with-font)))
          (if color
              (with-pen-color color with-size)
              with-size)))))

  (define (text-box x y str padding 
                    #!key (color #f) (bg-color #f) (font #f) (size #f))
    "Draw text with a background box"
    (combine
     ;; Background box (simplified - actual bbox calculation would be better)
     (if bg-color
         (rectangle (- x padding) (- y padding)
                    (+ (* (string-length str) 0.6) (* 2 padding))
                    (+ 1.0 (* 2 padding))
                    #:fill-color bg-color)
         empty-drawer)
     ;; Text
     (text x y str #:color color #:font font #:size size)))

  ;;; ========================================================================
  ;;; Utility Functions
  ;;; ========================================================================

  (define (move-to x y)
    "Move drawing cursor to (x, y) without drawing"
    (make-drawer
     (lambda (plotter)
       (fmove plotter (exact->inexact x) (exact->inexact y)))))

  (define (clip-rect x y width height drawer)
    "Clip drawing to rectangular region"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       ;; Note: libplot doesn't have direct clipping, but we can use
       ;; the coordinate space to simulate it
       (fspace plotter (exact->inexact x) (exact->inexact y)
               (exact->inexact (+ x width))
               (exact->inexact (+ y height)))
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

  (define (save-restore drawer)
    "Wrap drawer in savestate/restorestate"
    (make-drawer
     (lambda (plotter)
       (savestate plotter)
       ((drawer-proc drawer) plotter)
       (restorestate plotter))))

) ; end module
