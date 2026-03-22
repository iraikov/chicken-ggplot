;;;; gg-guides.scm
;;;; Grammar of Graphics - Guides (Axes and Legends)
;;;;
;;;; Visual representations of scales: axes with ticks/labels, legends

(module gg-guides
  (;; Axis construction
   make-axis-bottom
   make-axis-top
   make-axis-left
   make-axis-right
   
   ;; Legend construction
   make-legend-continuous
   make-legend-discrete
   
   ;; Rendering (returns drawers)
   axis-drawer
   legend-drawer)

  (import scheme
          (chicken base)
          (chicken format)
          yasos
          srfi-1
          gg-backend
          gg-primitives-vge
          gg-scales)

  ;;; -----------------------------------------------------------------------
  ;;; Bridge wrappers: translate old gg-primitives call conventions to the
  ;;; new VGE-based gg-primitives-vge API.
  ;;; -----------------------------------------------------------------------

  (define (line x1 y1 x2 y2 #!key (color "black") (width 1))
    (with-pen-color color
      (with-line-width (exact->inexact width)
        (line-drawer (exact->inexact x1) (exact->inexact y1)
                     (exact->inexact x2) (exact->inexact y2)))))

  ;; #:hjust 'left  -> right-align (right edge at x, for left-axis labels)
  ;; #:hjust 'right -> left-align  (left edge at x, for right-axis labels)
  ;; default        -> center-align
  (define (text x y str #!key (color "black") (size 10.0) (hjust 'center))
    (with-pen-color color
      (with-font "sans" (exact->inexact size) 'normal 'normal
        (text-drawer (exact->inexact x) (exact->inexact y) str
                     #:halign (case hjust
                                ((left)  halign/right)
                                ((right) halign/left)
                                (else    halign/center))
                     #:valign valign/center))))

  (define (rectangle x y w h #!key (fill-color "lightgray") (edge-color "black"))
    (filled-rect+border-drawer (exact->inexact x) (exact->inexact y)
                               (exact->inexact w) (exact->inexact h)
                               fill-color edge-color))

  ;; VGE has no rotation transform; render child unrotated.
  ;; Axis titles on vertical axes will appear horizontally — a known
  ;; limitation until VGE gains a gfx:rotate instruction.
  (define (with-rotate angle drawer) drawer)

  ;;; ========================================================================
  ;;; Axis Abstraction
  ;;; ========================================================================
  ;;;
  ;;; An axis is a visual representation of a scale, consisting of:
  ;;;   - Axis line
  ;;;   - Tick marks
  ;;;   - Tick labels
  ;;;   - Axis label (title)

  (define-record-type axis-config
    (make-axis-config-internal scale position label tick-count 
                               tick-length tick-width label-size 
                               label-offset line-color)
    axis-config?
    (scale axis-scale)
    (position axis-position)           ; 'bottom, 'top, 'left, 'right
    (label axis-label)                 ; Axis title
    (tick-count axis-tick-count)       ; Number of ticks
    (tick-length axis-tick-length)     ; Length of tick marks
    (tick-width axis-tick-width)       ; Width of tick lines
    (label-size axis-label-size)       ; Font size for labels
    (label-offset axis-label-offset)   ; Distance from axis
    (line-color axis-line-color))      ; Color of axis line and ticks

  (define (make-axis scale position
                     #!key (label "") (tick-count 5) (tick-length 5)
                     (tick-width 1) (label-size 10.0)
                     (label-offset 15) (line-color "black"))
    "Create an axis configuration"
    (make-axis-config-internal scale position label tick-count 
                               tick-length tick-width label-size 
                               label-offset line-color))

  (define (make-axis-bottom scale #!key (label "") (tick-count 5))
    "Create a bottom (x) axis"
    (make-axis scale 'bottom #:label label #:tick-count tick-count))

  (define (make-axis-top scale #!key (label "") (tick-count 5))
    "Create a top (x) axis"
    (make-axis scale 'top #:label label #:tick-count tick-count))

  (define (make-axis-left scale #!key (label "") (tick-count 5))
    "Create a left (y) axis"
    (make-axis scale 'left #:label label #:tick-count tick-count))

  (define (make-axis-right scale #!key (label "") (tick-count 5))
    "Create a right (y) axis"
    (make-axis scale 'right #:label label #:tick-count tick-count))

  ;;; ========================================================================
  ;;; Axis Rendering
  ;;; ========================================================================

  (define (axis-drawer axis-cfg)
    "Generate a drawer for the axis"
    (let* ((scale (axis-scale axis-cfg))
           (pos (axis-position axis-cfg))
           (all-breaks (scale-breaks scale (axis-tick-count axis-cfg)))
           (all-labels (scale-labels scale all-breaks))
           (range (scale-range scale))
           (r-min (car range))
           (r-max (cdr range))
           ;; Clip breaks to those whose mapped pixel position lies within
           ;; [r-min, r-max].  Prevents stray ticks outside the panel when
           ;; the scale domain does not already cover the nice-break range
           ;; (e.g. when using the low-level geom API without pre-expansion).
           (break-label-pairs
            (filter (lambda (bl)
                      (let ((mapped (scale-map scale (car bl))))
                        (and (>= mapped (- r-min 0.5))
                             (<= mapped (+ r-max 0.5)))))
                    (map cons all-breaks all-labels)))
           (breaks (map car break-label-pairs))
           (labels (map cdr break-label-pairs))
           (tick-len (axis-tick-length axis-cfg))
           (tick-w (axis-tick-width axis-cfg))
           (lbl-size (axis-label-size axis-cfg))
           (lbl-offset (axis-label-offset axis-cfg))
           (color (axis-line-color axis-cfg)))
      
      (case pos
        ;; Bottom axis (x)
        ((bottom)
         (combine
          ;; Axis line
          (line r-min 0 r-max 0 #:color color #:width tick-w)
          ;; Tick marks and labels
          (apply combine
                 (map (lambda (break label)
                        (let ((x (scale-map scale break)))
                          (combine
                           ;; Tick mark
                           (line x 0 x (- tick-len) #:color color #:width tick-w)
                           ;; Tick label
                           (text x (- lbl-offset) label 
                                 #:color color #:size lbl-size))))
                      breaks labels))
          ;; Axis label
          (if (string=? (axis-label axis-cfg) "")
              empty-drawer
              (text (/ (+ r-min r-max) 2) (- (* lbl-offset 2.5))
                    (axis-label axis-cfg)
                    #:color color #:size (* lbl-size 1.2)))))
        
        ;; Top axis (x)
        ((top)
         (combine
          (line r-min 0 r-max 0 #:color color #:width tick-w)
          (apply combine
                 (map (lambda (break label)
                        (let ((x (scale-map scale break)))
                          (combine
                           (line x 0 x tick-len #:color color #:width tick-w)
                           (text x lbl-offset label 
                                 #:color color #:size lbl-size))))
                      breaks labels))
          (if (string=? (axis-label axis-cfg) "")
              empty-drawer
              (text (/ (+ r-min r-max) 2) (* lbl-offset 2.5)
                    (axis-label axis-cfg)
                    #:color color #:size (* lbl-size 1.2)))))
        
        ;; Left axis (y)
        ((left)
         ;; Estimate the pixel width of the widest tick label so the axis title
         ;; can be placed just clear of all tick labels regardless of label length.
         ;; Character width ~= 0.65 * font-size for typical serif/proportional fonts.
         (let* ((max-lbl-w (if (null? labels)
                               0
                               (* (apply max (map string-length labels))
                                  lbl-size 0.65)))
                ;; title-x: left of tick-label left-edge by one lbl-size gap
                (title-x   (- (+ lbl-offset max-lbl-w lbl-size))))
           (combine
            (line 0 r-min 0 r-max #:color color #:width tick-w)
            (apply combine
                   (map (lambda (break label)
                          (let ((y (scale-map scale break)))
                            (combine
                             (line 0 y (- tick-len) y #:color color #:width tick-w)
                             ;; Right edge of label at -(lbl-offset)
                             (text (- lbl-offset) y label
                                   #:hjust 'left #:color color #:size lbl-size))))
                        breaks labels))
            (if (string=? (axis-label axis-cfg) "")
                empty-drawer
                (with-translate title-x (/ (+ r-min r-max) 2)
                                (with-rotate (/ 3.14159 2)
                                             (text 0 0 (axis-label axis-cfg)
                                                   #:color color #:size (* lbl-size 1.2))))))))

        ;; Right axis (y)
        ((right)
         (let* ((max-lbl-w (if (null? labels)
                               0
                               (* (apply max (map string-length labels))
                                  lbl-size 0.65)))
                (title-x   (+ lbl-offset max-lbl-w lbl-size)))
           (combine
            (line 0 r-min 0 r-max #:color color #:width tick-w)
            (apply combine
                   (map (lambda (break label)
                          (let ((y (scale-map scale break)))
                            (combine
                             (line 0 y tick-len y #:color color #:width tick-w)
                             ;; Left edge of label at +(lbl-offset)
                             (text lbl-offset y label
                                   #:hjust 'right #:color color #:size lbl-size))))
                        breaks labels))
            (if (string=? (axis-label axis-cfg) "")
                empty-drawer
                (with-translate title-x (/ (+ r-min r-max) 2)
                                (with-rotate (/ 3.14159 2)
                                             (text 0 0 (axis-label axis-cfg)
                                                   #:color color #:size (* lbl-size 1.2))))))))
        
        (else empty-drawer)))
    )

  ;;; ========================================================================
  ;;; Legend Abstraction
  ;;; ========================================================================

  (define-record-type legend-config
    (make-legend-config-internal scale title position 
                                 width height label-size)
    legend-config?
    (scale legend-scale)
    (title legend-title)
    (position legend-position)     ; (x . y) position
    (width legend-width)
    (height legend-height)
    (label-size legend-label-size))

  (define (make-legend-continuous scale 
                                  #!key (title "") (position '(0 . 0))
                                  (width 200) (height 30) (label-size 10.0))
    "Create a continuous legend (color bar)"
    (make-legend-config-internal scale title position width height label-size))

  (define (make-legend-discrete scale 
                                #!key (title "") (position '(0 . 0))
                                (width 150) (height 200) (label-size 10.0))
    "Create a discrete legend (swatches)"
    (make-legend-config-internal scale title position width height label-size))

  ;;; ========================================================================
  ;;; Legend Rendering
  ;;; ========================================================================

  (define (legend-drawer legend-cfg)
    "Generate a drawer for the legend"
    (let* ((scale (legend-scale legend-cfg))
           (title (legend-title legend-cfg))
           (pos (legend-position legend-cfg))
           (x (car pos))
           (y (cdr pos))
           (w (legend-width legend-cfg))
           (h (legend-height legend-cfg))
           (lbl-size (legend-label-size legend-cfg)))
      
      (with-translate x y
                      (combine
                       ;; Title
                       (if (string=? title "")
                           empty-drawer
                           (text 0 (+ h 20) title #:size (* lbl-size 1.2)))
                       
                       ;; Legend content
                       (let ((breaks (scale-breaks scale 5)))
                         (if (null? breaks)
                             empty-drawer
                             (apply combine
                                    (map (lambda (break idx)
                                           (let ((label (car (scale-labels scale (list break))))
                                                 (y-pos (* idx (/ h (length breaks))))
                                                 ;; Query scale for actual color
                                                 (swatch-color (scale-map scale break)))
                                             (combine
                                              ;; Swatch - use actual color from scale
                                              (rectangle 0 y-pos 20 (/ h (length breaks))
                                                         #:fill-color swatch-color
                                                         #:edge-color "black")
                                              ;; Label
                                              (text 25 (+ y-pos (/ (/ h (length breaks)) 2))
                                                    label #:size lbl-size))))
                                         breaks
                                         (iota (length breaks))))))))))
  
  ) ; end module
