;;;; gg-layout.scm
;;;; Grammar of Graphics - Advanced Layout
;;;;
;;;; Label collision avoidance, legend positioning, and annotation layers

(module gg-layout
  (;; Label collision detection and avoidance
   label-with-collision-avoidance
   detect-label-overlap
   force-directed-label-layout
   
   ;; Advanced legend positioning
   legend-position-auto
   legend-position-inside
   legend-position-outside
   compute-legend-bounds
   
   ;; Annotation layers
   annotate-text
   annotate-rect
   annotate-segment
   annotate-arrow
   annotate-curve
   render-annotation
   
   ;; Layout utilities
   bounding-box
   bbox-overlap?
   bbox-union
   bbox-center
   bbox-area
   compute-text-bounds
   make-bbox

   bbox-x-min
   bbox-y-min
   bbox-x-max
   bbox-y-max

   make-label-spec
   label-spec?
   label-text
   label-x
   label-y
   label-bbox
   label-size
   label-priority

   )

  (import scheme
          (chicken base)
          (chicken format)
          (chicken sort)
          yasos
          srfi-1
          srfi-13
          srfi-69
          matchable
          datatype
          gg-primitives-vge
          gg-scales)


  (define (assq-ref alist key)
    "Safe assq reference"
    (and (list? alist)
         (if (symbol? (car alist))
             (assq-ref (cdr alist) key)
             (let ((pair (assq key alist)))
               (if pair (cdr pair) #f)))))
  
  (define (assq-ref/dflt alist key dflt)
    "Safe assq reference with default"
    (or (assq-ref alist key) dflt))

  ;;; ========================================================================
  ;;; Local bridge: old keyword-arg primitive API -> new VGE drawer combinators
  ;;; ========================================================================

  (define (text x y str #!key (color "black") (size 10.0))
    (with-pen-color color
      (with-font "sans" size 'normal 'normal
        (text-drawer x y str))))

  (define (rectangle x y w h #!key (fill-color "lightgray") (edge-color "black")
                               (line-width 1) (alpha 1.0))
    (with-pen-color edge-color
      (with-fill-color fill-color
        (filled-rect-drawer x y w h))))

  (define (line x1 y1 x2 y2 #!key (color "black") (width 1))
    (with-pen-color color
      (with-line-width width
        (line-drawer x1 y1 x2 y2))))

  (define (polygon pts #!key (fill-color "lightgray") (edge-color "black"))
    (with-pen-color edge-color
      (with-fill-color fill-color
        (filled-polygon-drawer pts))))

  ;;; ========================================================================
  ;;; Bounding Box Utilities
  ;;; ========================================================================
  
  ;; Bounding box: (x-min y-min x-max y-max)
  (define-record-type bbox
    (make-bbox x-min y-min x-max y-max)
    bbox?
    (x-min bbox-x-min)
    (y-min bbox-y-min)
    (x-max bbox-x-max)
    (y-max bbox-y-max))
  
  (define (bounding-box x y width height)
    "Create a bounding box from position and dimensions"
    (make-bbox x y (+ x width) (+ y height)))
  
  (define (bbox-overlap? bbox1 bbox2)
    "Check if two bounding boxes overlap"
    (not (or (> (bbox-x-min bbox1) (bbox-x-max bbox2))
             (< (bbox-x-max bbox1) (bbox-x-min bbox2))
             (> (bbox-y-min bbox1) (bbox-y-max bbox2))
             (< (bbox-y-max bbox1) (bbox-y-min bbox2)))))
  
  (define (bbox-union bbox1 bbox2)
    "Compute the union of two bounding boxes"
    (make-bbox (min (bbox-x-min bbox1) (bbox-x-min bbox2))
               (min (bbox-y-min bbox1) (bbox-y-min bbox2))
               (max (bbox-x-max bbox1) (bbox-x-max bbox2))
               (max (bbox-y-max bbox1) (bbox-y-max bbox2))))
  
  (define (bbox-center bbox)
    "Get the center point of a bounding box"
    (cons (/ (+ (bbox-x-min bbox) (bbox-x-max bbox)) 2)
          (/ (+ (bbox-y-min bbox) (bbox-y-max bbox)) 2)))
  
  (define (bbox-area bbox)
    "Compute the area of a bounding box"
    (* (- (bbox-x-max bbox) (bbox-x-min bbox))
       (- (bbox-y-max bbox) (bbox-y-min bbox))))

  ;;; ========================================================================
  ;;; Text Bounds Estimation
  ;;; ========================================================================
  
  (define (compute-text-bounds text x y #!key (size 10.0) (angle 0))
    "Estimate bounding box for text label
     Uses simple heuristic: width = length * size * 0.6, height = size * 1.2"
    (let* ((char-width (* size 0.6))
           (char-height (* size 1.2))
           (width (* (string-length text) char-width))
           (height char-height)
           ;; Handle rotation (simplified - assumes horizontal for now)
           (half-w (/ width 2))
           (half-h (/ height 2)))
      (make-bbox (- x half-w) (- y half-h)
                 (+ x half-w) (+ y half-h))))

  ;;; ========================================================================
  ;;; Label Collision Detection
  ;;; ========================================================================
  
  (define-record-type label-spec
    (make-label-spec-internal text x y bbox size priority)
    label-spec?
    (text label-text)
    (x label-x label-x-set!)
    (y label-y label-y-set!)
    (bbox label-bbox label-bbox-set!)
    (size label-size)
    (priority label-priority))  ; Higher priority labels are preserved
  
  (define (make-label-spec text x y #!key (size 10.0) (priority 1.0))
    "Create a label specification with bounding box"
    (let ((bbox (compute-text-bounds text x y #:size size)))
      (make-label-spec-internal text x y bbox size priority)))
  
  (define (detect-label-overlap labels)
    "Detect overlapping label pairs
     Returns: list of (label-a label-b overlap-area) tuples"
    (let loop ((remaining labels)
               (overlaps '()))
      (if (null? remaining)
          overlaps
          (let* ((current (car remaining))
                 (rest (cdr remaining))
                 (current-overlaps
                  (filter-map
                   (lambda (other)
                     (if (bbox-overlap? (label-bbox current) (label-bbox other))
                         (let* ((intersection
                                 (make-bbox
                                  (max (bbox-x-min (label-bbox current))
                                       (bbox-x-min (label-bbox other)))
                                  (max (bbox-y-min (label-bbox current))
                                       (bbox-y-min (label-bbox other)))
                                  (min (bbox-x-max (label-bbox current))
                                       (bbox-x-max (label-bbox other)))
                                  (min (bbox-y-max (label-bbox current))
                                       (bbox-y-max (label-bbox other)))))
                                (area (bbox-area intersection)))
                           (list current other area))
                         #f))
                   rest)))
            (loop rest (append overlaps current-overlaps))))))

  ;;; ========================================================================
  ;;; Force-Directed Label Layout
  ;;; ========================================================================
  
  (define (force-directed-label-layout labels
                                       #!key (iterations 50)
                                            (k-repel 100.0)
                                            (k-attract 0.1)
                                            (damping 0.8))
    "Apply force-directed algorithm to resolve label overlaps
     
     Forces:
       - Repulsion between overlapping labels
       - Weak attraction to original position
       - Damping to prevent oscillation
     
     Returns: list of adjusted label-specs"
    
    ;; Store original positions for attraction force
    (define orig-positions
      (map (lambda (lbl)
             (cons (label-x lbl) (label-y lbl)))
           labels))
    
    ;; Iterate force simulation
    (let iter ((labels labels)
               (step 0))
      (if (>= step iterations)
          labels
          (let* ((overlaps (detect-label-overlap labels))
                 ;; Compute forces for each label
                 (forces (map (lambda (lbl orig-pos)
                                (let* ((x (label-x lbl))
                                       (y (label-y lbl))
                                       (orig-x (car orig-pos))
                                       (orig-y (cdr orig-pos))
                                       ;; Attraction to original position
                                       (attract-fx (* k-attract (- orig-x x)))
                                       (attract-fy (* k-attract (- orig-y y)))
                                       ;; Repulsion from overlapping labels
                                       (repel-forces
                                        (filter-map
                                         (lambda (overlap)
                                           (cond
                                            ((eq? lbl (car overlap))
                                             ;; Repel from second label
                                             (let* ((other (cadr overlap))
                                                    (dx (- x (label-x other)))
                                                    (dy (- y (label-y other)))
                                                    (dist (sqrt (+ (* dx dx) (* dy dy))))
                                                    (safe-dist (max dist 1.0))
                                                    (force (* k-repel (/ 1.0 safe-dist))))
                                               (cons (* force (/ dx safe-dist))
                                                     (* force (/ dy safe-dist)))))
                                            ((eq? lbl (cadr overlap))
                                             ;; Repel from first label
                                             (let* ((other (car overlap))
                                                    (dx (- x (label-x other)))
                                                    (dy (- y (label-y other)))
                                                    (dist (sqrt (+ (* dx dx) (* dy dy))))
                                                    (safe-dist (max dist 1.0))
                                                    (force (* k-repel (/ 1.0 safe-dist))))
                                               (cons (* force (/ dx safe-dist))
                                                     (* force (/ dy safe-dist)))))
                                            (else #f)))
                                         overlaps))
                                       (total-repel-fx (apply + 0 (map car repel-forces)))
                                       (total-repel-fy (apply + 0 (map cdr repel-forces))))
                                  (cons (+ attract-fx total-repel-fx)
                                        (+ attract-fy total-repel-fy))))
                              labels
                              orig-positions))
                 ;; Apply forces with damping
                 (updated-labels
                  (map (lambda (lbl force)
                         (let* ((new-x (+ (label-x lbl) (* damping (car force))))
                                (new-y (+ (label-y lbl) (* damping (cdr force))))
                                (new-bbox (compute-text-bounds
                                           (label-text lbl)
                                           new-x new-y
                                           #:size (label-size lbl))))
                           (label-x-set! lbl new-x)
                           (label-y-set! lbl new-y)
                           (label-bbox-set! lbl new-bbox)
                           lbl))
                       labels
                       forces)))
            (iter updated-labels (+ step 1))))))

  ;;; ========================================================================
  ;;; High-Level Label Layout
  ;;; ========================================================================
  
  (define (label-with-collision-avoidance labels-data
                                          #!key (avoid-overlap? #t)
                                               (max-iterations 50))
    "Create labels with automatic collision avoidance
     
     labels-data: list of (text x y #:size size #:priority priority) specs
     Returns: drawer that renders non-overlapping labels"
    
    (if (not avoid-overlap?)
        ;; Simple rendering without adjustment
        (apply combine
               (map (lambda (spec)
                      (apply text spec))
                    labels-data))
        ;; Force-directed layout
        (let* ((label-specs
                (map (lambda (spec)
                       (match spec
                         ((text x y #:size size #:priority prio)
                          (make-label-spec text x y #:size size #:priority prio))
                         ((text x y #:size size)
                          (make-label-spec text x y #:size size))
                         ((text x y)
                          (make-label-spec text x y))))
                     labels-data))
               (adjusted-specs
                (force-directed-label-layout label-specs
                                             #:iterations max-iterations)))
          (apply combine
                 (map (lambda (spec)
                        (text (label-x spec)
                              (label-y spec)
                              (label-text spec)
                              #:size (label-size spec)))
                      adjusted-specs)))))

  ;;; ========================================================================
  ;;; Legend Positioning
  ;;; ========================================================================
  
  (define (compute-legend-bounds legend-spec)
    "Compute bounding box for a legend
     Returns: bbox"
    (match legend-spec
      (('discrete title items width height)
       (bounding-box 0 0 width height))
      (('continuous title width height)
       (bounding-box 0 0 width height))))
  
  (define (legend-position-auto legends plot-bbox panel-bbox)
    "Automatically position legends to minimize overlap with plot content
     
     Strategy:
       1. Try right margin first (default)
       2. If overlaps, try left margin
       3. If overlaps, try top margin
       4. If overlaps, try bottom margin
       
     Returns: list of positioned legends with (x . y) coords"
    
    (let* ((plot-width (- (bbox-x-max plot-bbox) (bbox-x-min plot-bbox)))
           (plot-height (- (bbox-y-max plot-bbox) (bbox-y-min plot-bbox)))
           (right-x (+ (bbox-x-max panel-bbox) 20))
           (left-x (- (bbox-x-min panel-bbox) 200))
           (top-y (bbox-y-max panel-bbox))
           (bottom-y (bbox-y-min panel-bbox)))
      
      ;; For now, stack legends vertically on the right
      (let loop ((legends legends)
                 (y-offset top-y)
                 (positioned '()))
        (if (null? legends)
            (reverse positioned)
            (let* ((current (car legends))
                   (bbox (compute-legend-bounds current))
                   (height (- (bbox-y-max bbox) (bbox-y-min bbox))))
              (loop (cdr legends)
                    (- y-offset height 20)
                    (cons (cons current (cons right-x y-offset))
                          positioned)))))))
  
  (define (legend-position-inside legends position panel-bbox)
    "Position legends inside the plot panel
     
     position: 'top-left, 'top-right, 'bottom-left, 'bottom-right, 'center
     Returns: list of positioned legends"
    
    (let* ((panel-width (- (bbox-x-max panel-bbox) (bbox-x-min panel-bbox)))
           (panel-height (- (bbox-y-max panel-bbox) (bbox-y-min panel-bbox)))
           (margin 10))
      
      (map (lambda (legend)
             (let ((bbox (compute-legend-bounds legend)))
               (case position
                 ((top-left)
                  (cons legend
                        (cons (+ (bbox-x-min panel-bbox) margin)
                              (- (bbox-y-max panel-bbox) margin
                                 (- (bbox-y-max bbox) (bbox-y-min bbox))))))
                 ((top-right)
                  (cons legend
                        (cons (- (bbox-x-max panel-bbox) margin
                                 (- (bbox-x-max bbox) (bbox-x-min bbox)))
                              (- (bbox-y-max panel-bbox) margin
                                 (- (bbox-y-max bbox) (bbox-y-min bbox))))))
                 ((bottom-left)
                  (cons legend
                        (cons (+ (bbox-x-min panel-bbox) margin)
                              (+ (bbox-y-min panel-bbox) margin))))
                 ((bottom-right)
                  (cons legend
                        (cons (- (bbox-x-max panel-bbox) margin
                                 (- (bbox-x-max bbox) (bbox-x-min bbox)))
                              (+ (bbox-y-min panel-bbox) margin))))
                 ((center)
                  (cons legend
                        (cons (- (/ (+ (bbox-x-min panel-bbox) (bbox-x-max panel-bbox)) 2)
                                 (/ (- (bbox-x-max bbox) (bbox-x-min bbox)) 2))
                              (- (/ (+ (bbox-y-min panel-bbox) (bbox-y-max panel-bbox)) 2)
                                 (/ (- (bbox-y-max bbox) (bbox-y-min bbox)) 2)))))
                 (else
                  (cons legend (cons (bbox-x-min panel-bbox) (bbox-y-max panel-bbox)))))))
           legends)))
  
  (define (legend-position-outside legends position panel-bbox plot-bbox)
    "Position legends outside the plot panel in margins
     
     position: 'right, 'left, 'top, 'bottom
     Returns: list of positioned legends"
    
    (case position
      ((right)
       (legend-position-auto legends plot-bbox panel-bbox))
      ((left)
       (let ((x (- (bbox-x-min panel-bbox) 200)))
         (let loop ((legends legends)
                    (y-offset (bbox-y-max panel-bbox))
                    (positioned '()))
           (if (null? legends)
               (reverse positioned)
               (let* ((current (car legends))
                      (bbox (compute-legend-bounds current))
                      (height (- (bbox-y-max bbox) (bbox-y-min bbox))))
                 (loop (cdr legends)
                       (- y-offset height 20)
                       (cons (cons current (cons x y-offset))
                             positioned)))))))
      ((top bottom)
       ;; Horizontal layout for top/bottom
       (let ((y (if (eq? position 'top)
                    (+ (bbox-y-max panel-bbox) 20)
                    (- (bbox-y-min panel-bbox) 100))))
         (let loop ((legends legends)
                    (x-offset (bbox-x-min panel-bbox))
                    (positioned '()))
           (if (null? legends)
               (reverse positioned)
               (let* ((current (car legends))
                      (bbox (compute-legend-bounds current))
                      (width (- (bbox-x-max bbox) (bbox-x-min bbox))))
                 (loop (cdr legends)
                       (+ x-offset width 20)
                       (cons (cons current (cons x-offset y))
                             positioned)))))))
      (else
       (legend-position-auto legends plot-bbox panel-bbox))))

  ;;; ========================================================================
  ;;; Annotation Layers
  ;;; ========================================================================
  
  (define (annotate-text text x y #!key (size 10.0) (color "black")
                         (hjust 0.5) (vjust 0.5) (angle 0))
    "Create a text annotation at absolute coordinates
     
     hjust, vjust: horizontal/vertical justification (0=left/bottom, 1=right/top)
     Returns: annotation spec for rendering"
    `(annotation text
                 (text . ,text)
                 (x . ,x)
                 (y . ,y)
                 (size . ,size)
                 (color . ,color)
                 (hjust . ,hjust)
                 (vjust . ,vjust)
                 (angle . ,angle)))
  
  (define (annotate-rect xmin ymin xmax ymax
                         #!key (fill "gray") (alpha 0.3)
                              (color "none") (line-width 0))
    "Create a rectangular annotation
     
     Useful for highlighting regions (e.g., stimulus periods)
     Returns: annotation spec"
    `(annotation rect
                 (xmin . ,xmin)
                 (ymin . ,ymin)
                 (xmax . ,xmax)
                 (ymax . ,ymax)
                 (fill . ,fill)
                 (alpha . ,alpha)
                 (color . ,color)
                 (line-width . ,line-width)))
  
  (define (annotate-segment x y xend yend
                            #!key (color "black") (line-width 1)
                                 (linetype 'solid) (arrow? #f))
    "Create a line segment annotation
     
     arrow?: if #t, draw arrowhead at endpoint
     Returns: annotation spec"
    `(annotation segment
                 (x . ,x)
                 (y . ,y)
                 (xend . ,xend)
                 (yend . ,yend)
                 (color . ,color)
                 (line-width . ,line-width)
                 (linetype . ,linetype)
                 (arrow? . ,arrow?)))
  
  (define (annotate-arrow x y xend yend
                          #!key (color "black") (line-width 1)
                          (arrow-length 10) (arrow-angle 30))
    "Create an arrow annotation
     
     arrow-length: length of arrowhead in pixels
     arrow-angle: angle of arrowhead in degrees
     Returns: annotation spec"
    `(annotation arrow
                 (x . ,x)
                 (y . ,y)
                 (xend . ,xend)
                 (yend . ,yend)
                 (color . ,color)
                 (line-width . ,line-width)
                 (arrow-length . ,arrow-length)
                 (arrow-angle . ,arrow-angle)))
  
  (define (annotate-curve x y xend yend
                          #!key (curvature 0.5) (color "black")
                          (line-width 1) (arrow? #f))
    "Create a curved line annotation
     
     curvature: controls bend amount (-1 to 1, negative = left, positive = right)
     Returns: annotation spec"
    `(annotation curve
                 (x . ,x)
                 (y . ,y)
                 (xend . ,xend)
                 (yend . ,yend)
                 (curvature . ,curvature)
                 (color . ,color)
                 (line-width . ,line-width)
                 (arrow? . ,arrow?)))

  ;;; ========================================================================
  ;;; Annotation Rendering
  ;;; ========================================================================
  
  (define (render-annotation annotation scales)
    "Convert annotation spec to drawer
     
     Applies scale transformations to coordinates
     Returns: drawer"
    (match annotation
      (('annotation 'text params ...)
       (let ((text-val (assq-ref params 'text))
             (x (assq-ref params 'x))
             (y (assq-ref params 'y))
             (size (assq-ref/dflt params 'size 10.0))
             (color (assq-ref/dflt params 'color "black"))
             (angle (assq-ref/dflt params 'angle 0)))
         ;; Apply scale transformation
         (let ((x-scale (assq-ref scales 'x))
               (y-scale (assq-ref scales 'y)))
           (let ((px (if x-scale (scale-map x-scale x) x))
                 (py (if y-scale (scale-map y-scale y) y)))
             (text px py text-val #:size size #:color color)))))
      
      (('annotation 'rect params ...)
       (let ((xmin (assq-ref params 'xmin))
             (ymin (assq-ref params 'ymin))
             (xmax (assq-ref params 'xmax))
             (ymax (assq-ref params 'ymax))
             (fill (assq-ref/dflt params 'fill "gray"))
             (alpha (assq-ref/dflt params 'alpha 0.3))
             (color (assq-ref/dflt params 'color "none"))
             (lw (assq-ref/dflt params 'line-width 0)))
         (let ((x-scale (assq-ref scales 'x))
               (y-scale (assq-ref scales 'y)))
           (let ((px1 (if x-scale (scale-map x-scale xmin) xmin))
                 (py1 (if y-scale (scale-map y-scale ymin) ymin))
                 (px2 (if x-scale (scale-map x-scale xmax) xmax))
                 (py2 (if y-scale (scale-map y-scale ymax) ymax)))
             (rectangle px1 py1 (- px2 px1) (- py2 py1)
                        #:fill-color fill #:edge-color color
                        #:line-width lw #:alpha alpha)))))
      
      (('annotation 'segment params ...)
       (let ((x (assq-ref params 'x))
             (y (assq-ref params 'y))
             (xend (assq-ref params 'xend))
             (yend (assq-ref params 'yend))
             (color (assq-ref/dflt params 'color "black"))
             (lw (assq-ref/dflt params 'line-width 1)))
         (let ((x-scale (assq-ref scales 'x))
               (y-scale (assq-ref scales 'y)))
           (let ((px1 (if x-scale (scale-map x-scale x) x))
                 (py1 (if y-scale (scale-map y-scale y) y))
                 (px2 (if x-scale (scale-map x-scale xend) xend))
                 (py2 (if y-scale (scale-map y-scale yend) yend)))
             (line px1 py1 px2 py2 #:color color #:width lw)))))
      
      (('annotation 'arrow params ...)
       ;; Arrow implemented as line + triangle
       (let ((x (assq-ref params 'x))
             (y (assq-ref params 'y))
             (xend (assq-ref params 'xend))
             (yend (assq-ref params 'yend))
             (color (assq-ref/dflt params 'color "black"))
             (lw (assq-ref/dflt params 'line-width 1))
             (arrow-len (assq-ref/dflt params 'arrow-length 10)))
         (let ((x-scale (assq-ref scales 'x))
               (y-scale (assq-ref scales 'y)))
           (let* ((px1 (if x-scale (scale-map x-scale x) x))
                  (py1 (if y-scale (scale-map y-scale y) y))
                  (px2 (if x-scale (scale-map x-scale xend) xend))
                  (py2 (if y-scale (scale-map y-scale yend) yend))
                  (dx (- px2 px1))
                  (dy (- py2 py1))
                  (len (sqrt (+ (* dx dx) (* dy dy))))
                  (ux (/ dx len))
                  (uy (/ dy len))
                  ;; Arrowhead points
                  (arrow-base-x (- px2 (* ux arrow-len)))
                  (arrow-base-y (- py2 (* uy arrow-len)))
                  (perp-x (* (- uy) (/ arrow-len 2)))
                  (perp-y (* ux (/ arrow-len 2))))
             (combine
              ;; Line
              (line px1 py1 px2 py2 #:color color #:width lw)
              ;; Arrowhead (filled triangle)
              (polygon (list (cons px2 py2)
                             (cons (+ arrow-base-x perp-x)
                                   (+ arrow-base-y perp-y))
                             (cons (- arrow-base-x perp-x)
                                   (- arrow-base-y perp-y)))
                       #:fill-color color #:edge-color color))))))
      
      (else empty-drawer)))

  ) ; end module
