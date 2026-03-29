;;;; gg-plot.scm
;;;; Grammar of Graphics in Scheme
;;;;
;;;; Declarative plot system with s-expression representation

(module gg-plot
  (;; Plot construction
   ggplot
   layer
   
   ;; Layer constructors
   layer-point
   layer-line
   layer-path
   layer-area
   layer-bar
   layer-rect
   layer-text
   layer-segment
   layer-hline
   layer-vline
   layer-eventplot
   layer-histogram
   layer-density
   layer-boxplot
   layer-violin
   layer-errorbar
   layer-pointrange
   layer-linerange
   layer-crossbar
   layer-col
   layer-annotate-text
   layer-annotate-rect
   layer-annotate-segment
   layer-annotate-arrow
   
   ;; Plot specification
   plot-spec
   plot-spec?
   plot-spec-data
   plot-spec-default-aes
   plot-spec-layers
   plot-spec-scales
   plot-spec-coord
   plot-spec-facet
   plot-spec-theme
   plot-spec-labels
   
   ;; Layer operations
   make-layer
   layer?
   layer-geom
   layer-data
   layer-aes
   layer-params
   layer-stat
   
   ;; Plot rendering
   render-plot
   plot->sexp
   sexp->plot
   ggsave
   ggdisplay
   
   ;; Scale specifications
   scale-x-continuous
   scale-y-continuous
   scale-x-discrete
   scale-y-discrete
   scale-x-log
   scale-y-log
   scale-color-manual
   scale-color-gradient
   scale-fill-manual
   
   ;; Coordinate systems
   coord-cartesian
   coord-fixed
   coord-flip
   
   ;; Faceting
   facet-null
   facet-wrap
   facet-grid
   
   ;; Themes
   theme-gray
   theme-minimal
   theme-classic
   theme-bw
   theme-linedraw
   theme-light
   theme-dark
   theme-void
   
   ;; Labels
   labels
   labs
   xlab
   ylab
   ggtitle
   
   ;; Plot-level annotation helpers
   annotate-text-layer
   annotate-rect-layer
   annotate-vline
   annotate-hline
   annotate-abline
   
   ;; Legend control
   theme-legend-position
   theme-legend-inside
   theme-legend-outside
   
   ;; Label collision control
   geom-text-repel
   geom-label-repel

   )

  (import scheme
          (chicken base)
          (chicken format)
          (chicken string)
          (chicken keyword)
          srfi-1
          srfi-69
          statistics
          matchable
          yasos
          yasos-collections
          gg-vge
          gg-primitives-vge
          gg-backend
          gg-backend-cairo
          gg-scales
          gg-data
          gg-aes
          gg-geom
          gg-guides
          gg-layout)

  ;;; ========================================================================
  ;;; Data Structures
  ;;; ========================================================================

  (define-record plot-spec
    data           ; Dataset (columnar alist)
    default-aes    ; Default aesthetic mapping
    layers         ; List of layer records
    scales         ; Alist of scale specifications
    coord          ; Coordinate system specification
    facet          ; Faceting specification
    theme          ; Theme specification
    labels)        ; Axis/title labels

  (define-record layer
    geom           ; Geometry function name (symbol)
    data           ; Layer-specific data (or #f to inherit)
    aes            ; Layer-specific aesthetics
    params         ; Static parameters (alist)
    stat)          ; Statistical transformation (symbol or #f)

  ;;; ========================================================================
  ;;; Plot Construction API
  ;;; ========================================================================

  (define (ggplot data . specs)
    "Create a plot specification
     Usage: (ggplot data (aes ...) spec1 spec2 ...)
     
     Where specs can be:
     - (aes ...) - default aesthetic mapping
     - geometry layers
     - scale specifications
     - coordinate system
     - faceting
     - theme
     - labels"
    
    (let* ((default-aes (if (and (pair? specs)
                                 (aesthetic-mapping? (car specs)))
                           (car specs)
                           (aes)))
           (rest-specs (if (and (pair? specs)
                                (aesthetic-mapping? (car specs)))
                           (cdr specs)
                           specs)))
      
      ;; Build initial plot spec
      (let ((initial-spec (make-plot-spec
                           data
                           default-aes
                           '()              ; layers
                           '()              ; scales
                           (coord-cartesian) ; default coord
                           (facet-null)      ; no faceting
                           (theme-minimal)   ; default theme
                           '())))            ; labels
        
        ;; Process specification elements
        (fold (lambda (spec plot)
                (cond
                 ;; Annotation layer: returned by layer-annotate-*, annotate-vline,
                 ;; annotate-hline, annotate-abline.  These are plain s-expressions
                 ;; of the form (layer annotation ...) and do NOT pass (layer? spec),
                 ;; which is the record predicate.
                 ;;
                 ;; TODO: implement an algebraic datatype for plot layers that includes annotations.
                 ;;
                 ((and (pair? spec)
                       (eq? (car spec) 'layer)
                       (pair? (cdr spec))
                       (eq? (cadr spec) 'annotation))
                  (plot-spec-layers-set! plot
                                         (append (plot-spec-layers plot) (list spec)))
                  plot)
         
                 ;; Layer
                 ((layer? spec)
                  (plot-spec-layers-set! plot (append (plot-spec-layers plot)
                                                      (list spec)))
                  plot)
                 
                 ;; Scale specification
                 ((and (pair? spec) (memq (car spec) '(scale-x scale-y scale-color
                                                               scale-fill scale-alpha)))
                  (plot-spec-scales-set! plot (cons spec (plot-spec-scales plot)))
                  plot)
                 
                 ;; Coordinate system
                 ((and (pair? spec) (eq? (car spec) 'coord))
                  (plot-spec-coord-set! plot spec)
                  plot)
                 
                 ;; Faceting
                 ((and (pair? spec) (eq? (car spec) 'facet))
                  (plot-spec-facet-set! plot spec)
                  plot)
                 
                 ;; Theme
                 ((and (pair? spec) (eq? (car spec) 'theme))
                  (plot-spec-theme-set! plot spec)
                  plot)
                 
                 ;; Labels
                 ((and (pair? spec) (eq? (car spec) 'labs))
                  (plot-spec-labels-set! plot (append (plot-spec-labels plot)
                                                      (cdr spec)))
                  plot)
                 
                 (else plot)))
              initial-spec
              rest-specs))))

  ;;; ========================================================================
  ;;; Layer Construction
  ;;; ========================================================================

  (define (keywords->alist keyword-list)
  "Convert flat keyword argument list to alist.
   
   Input:  (#:x 1 #:y 2 #:color \"red\")
   Output: ((x . 1) (y . 2) (color . \"red\"))"
  
  (let loop ((kws keyword-list)
             (acc '()))
    (cond
     ;; Base case: no more keywords
     ((null? kws)
      (reverse acc))
     
     ;; Error case: odd number of arguments
     ((null? (cdr kws))
      (error "keywords->alist: odd number of keyword arguments" keyword-list))
     
     ;; Recursive case: process key-value pair
     (else
      (let ((key (car kws))
            (val (cadr kws)))
        ;; Convert keyword to symbol
        (if (keyword? key)
            (let ((sym (string->symbol (keyword->string key))))
              (loop (cddr kws)
                    (cons (cons sym val) acc)))
            (error "keywords->alist: expected keyword, got" key)))))))
  
  (define (layer geom-name #!rest params #!key (data #f) (mapping (aes)) (stat #f) )
    "Create a layer specification
     
     geom-name: Symbol naming geometry (e.g., 'point, 'line)
     data: Layer-specific data (inherits if #f)
     mapping: Aesthetic mapping (merged with plot default)
     stat: Statistical transformation
     params: Named parameters passed to geometry"

    (let ((params-alist (if (null? params)
                          '()
                          (keywords->alist params))))
    
      (make-layer geom-name
                  data
                  mapping
                  params-alist
                  stat)))

  ;;; ========================================================================
  ;;; Convenience Layer Constructors
  ;;; ========================================================================

  ;; These mirror the geom-* functions but return layer specs instead of drawers
  
  (define-syntax define-layer-constructor
    (syntax-rules ()
      ((_ name geom-sym)
       (define (name #!rest params #!key (data #f) (mapping (aes)) (stat #f) )
         (apply layer 'geom-sym #:data data #:mapping mapping #:stat stat params)))))

  ;; Generate layer constructors for all implemented geometries
  (define-layer-constructor layer-point point)
  (define-layer-constructor layer-line line)
  (define-layer-constructor layer-path path)
  (define-layer-constructor layer-area area)
  (define-layer-constructor layer-bar bar)
  (define-layer-constructor layer-rect rect)
  (define-layer-constructor layer-text text)
  (define-layer-constructor layer-segment segment)
  (define-layer-constructor layer-eventplot eventplot)

  ; Reference lines (hline/vline) need specialized constructors
  ;; because they don't use data like other geometries
  
  (define (layer-hline yintercept #!key (color "black") (width 1) 
                       (linetype 'solid) (alpha 1.0))
    "Create horizontal reference line layer at y-intercept"
    (make-layer 'hline
                #f  ; No data
                (aes)  ; No aesthetic mapping
                (list (cons 'yintercept yintercept)
                      (cons 'color color)
                      (cons 'width width)
                      (cons 'linetype linetype)
                      (cons 'alpha alpha))
                #f))  ; No stat
  
  (define (layer-vline xintercept #!key (color "black") (width 1)
                       (linetype 'solid) (alpha 1.0))
    "Create vertical reference line layer at x-intercept"
    (make-layer 'vline
                #f  ; No data
                (aes)  ; No aesthetic mapping
                (list (cons 'xintercept xintercept)
                      (cons 'color color)
                      (cons 'width width)
                      (cons 'linetype linetype)
                      (cons 'alpha alpha))
                #f))  ; No stat
  

;;;; ========================================================================
;;;; Statistical Layer Constructors
;;;; ========================================================================

;;; These constructors provide the user-facing API for statistical graphics.
;;; Each constructor:
;;;   1. Accepts statistical parameters (bins, bandwidth, etc.)
;;;   2. Creates a layer with appropriate stat transformation
;;;   3. Passes aesthetic mappings and other params to geometry

;;; ------------------------------------------------------------------------
;;; layer-histogram: Histogram with automatic binning
;;; ------------------------------------------------------------------------

  (define (layer-histogram #!key (data #f) (mapping (aes)) 
                           (bins 30) (binwidth #f) (breaks #f)
                           (method 'sturges) 
                           (fill "steelblue") (color "black") (alpha 0.9))
    "Create histogram layer with automatic binning
   
     Statistical parameters:
       bins:     Number of bins (default 30, ignored if breaks specified)
       binwidth: Width of bins in data units (overrides bins)
       breaks:   Explicit vector of breakpoints
       method:   Binning method ('sturges, 'scott, 'fd, 'equal-width)
   
     Aesthetic parameters:
       fill:     Fill color for bars
       color:    Edge color for bars
       alpha:    Transparency (0-1)
   
     Required aesthetics in mapping: x
   
     Example:
       (layer-histogram #:mapping (aes #:x 'sepal_length) 
                        #:bins 20 #:fill \"steelblue\")"
  
    (make-layer 'histogram 
                data 
                mapping
                `((bins . ,bins)
                  (binwidth . ,binwidth)
                  (breaks . ,breaks)
                  (method . ,method)
                  (fill . ,fill)
                  (color . ,color)
                  (alpha . ,alpha))
                'stat-bin))

;;; ------------------------------------------------------------------------
;;; layer-density: Kernel density estimation plot
;;; ------------------------------------------------------------------------

  (define (layer-density #!key (data #f) (mapping (aes))
                         (bandwidth #f) (kernel 'gaussian)
                         (n 512) (adjust 1.0)
                         (geom-type 'area)
                         (fill "lightblue") (color "navy") (alpha 0.5))
    "Create density plot layer with kernel density estimation
   
     Statistical parameters:
       bandwidth: KDE bandwidth (auto-selected if #f using NRD method)
       kernel:    Kernel function ('gaussian, 'epanechnikov, 'rectangular,
                                   'triangular, 'biweight)
       n:         Number of evaluation points (default 512)
       adjust:    Bandwidth adjustment factor (default 1.0)
   
     Aesthetic parameters:
       geom-type: 'area (filled) or 'line (curve only)
       fill:      Fill color for area
       color:     Line/edge color
       alpha:     Transparency (0-1)
   
     Required aesthetics in mapping: x
   
     Example:
       (layer-density #:mapping (aes #:x 'petal_length)
                      #:kernel 'gaussian #:fill \"steelblue\")"
  
    (make-layer 'density
                data
                mapping
                `((bandwidth . ,bandwidth)
                  (kernel . ,kernel)
                  (n . ,n)
                  (adjust . ,adjust)
                  (geom-type . ,geom-type)
                  (fill . ,fill)
                  (color . ,color)
                  (alpha . ,alpha))
                'stat-density))
  
;;; ------------------------------------------------------------------------
;;; layer-boxplot: Box plot with quartile computation
;;; ------------------------------------------------------------------------

  (define (layer-boxplot #!key (data #f) (mapping (aes))
                         (coef 1.5) (varwidth #f)
                         (fill "white") (color "black") 
                         (outlier-color "red") (outlier-size 3)
                         (width 0.6))
   "Create box plot layer with automatic quartile computation
   
    Statistical parameters:
      coef:     IQR multiplier for whisker length (default 1.5)
      varwidth: Scale box width by sample size (#f = constant width)
   
    Aesthetic parameters:
      fill:          Fill color for box
      color:         Edge color for box and whiskers
      outlier-color: Color for outlier points
      outlier-size:  Size of outlier points
      width:         Box width relative to group spacing (0-1)
   
    Required aesthetics in mapping: x (group), y (values)
   
    Example:
      (layer-boxplot #:mapping (aes #:x 'species #:y 'petal_length)
                     #:fill \"lightgray\" #:outlier-color \"red\")"
  
   (make-layer 'boxplot
               data
               mapping
               `((coef . ,coef)
                 (varwidth . ,varwidth)
                 (fill . ,fill)
                 (color . ,color)
                 (outlier-color . ,outlier-color)
                 (outlier-size . ,outlier-size)
                 (width . ,width))
               'stat-boxplot))
  
;;; ------------------------------------------------------------------------
;;; layer-violin: Violin plot (density + boxplot)
;;; ------------------------------------------------------------------------

  (define (layer-violin #!key (data #f) (mapping (aes))
                        (scale 'area) (draw-quantiles #f)
                        (bandwidth #f) (kernel 'gaussian)
                        (fill "lightgray") (color "black") (alpha 0.7))
   "Create violin plot layer
   
    Statistical parameters:
      scale:          Scaling method ('area = equal area, 'count = proportional to n)
      draw-quantiles: List of quantiles to draw as lines (e.g., '(0.25 0.5 0.75))
      bandwidth:      KDE bandwidth (auto-selected if #f)
      kernel:         Kernel function for density estimation
   
    Aesthetic parameters:
      fill:  Fill color for violin
      color: Edge color
      alpha: Transparency (0-1)
   
    Required aesthetics in mapping: x (group), y (values)
   
    Example:
      (layer-violin #:mapping (aes #:x 'species #:y 'petal_length)
                    #:draw-quantiles '(0.25 0.5 0.75))"
  
   (make-layer 'violin
               data
               mapping
               `((scale . ,scale)
                 (draw-quantiles . ,draw-quantiles)
                 (bandwidth . ,bandwidth)
                 (kernel . ,kernel)
                 (fill . ,fill)
                 (color . ,color)
                 (alpha . ,alpha))
               'stat-violin))
  
;;; ------------------------------------------------------------------------
;;; layer-errorbar: Error bars with optional computation
;;; ------------------------------------------------------------------------

  (define (layer-errorbar #!key (data #f) (mapping (aes))
                          (stat 'identity) (fun-ymin #f) (fun-ymax #f)
                          (color "black") (width 1.5) (cap-width 0.4))
    "Create error bar layer
   
     Two modes of operation:
    
     1. Pre-computed intervals (stat = 'identity):
        Mapping must include ymin and ymax aesthetics
        Example: (aes #:x 'group #:y 'mean #:ymin 'lower #:ymax 'upper)
   
     2. Computed intervals (stat = 'summary):
        Provide fun-ymin and fun-ymax functions to compute bounds
        Example: (aes #:x 'group #:y 'values) with custom functions
   
     Statistical parameters:
       stat:      'identity (use ymin/ymax from data) or 
                  'summary (compute from y values)
       fun-ymin:  Function to compute lower bound (for stat='summary)
       fun-ymax:  Function to compute upper bound (for stat='summary)
   
     Aesthetic parameters:
       color:     Line color
       width:     Line width
       cap-width: Width of end caps relative to data range
   
     Example (pre-computed):
       (layer-errorbar #:mapping (aes #:x 'group #:y 'mean 
                                      #:ymin 'ci_lower #:ymax 'ci_upper))
   
     Example (computed - mean ± SE):
       (layer-errorbar #:mapping (aes #:x 'group #:y 'values)
                       #:stat 'summary
                       #:fun-ymin (lambda (vals) 
                                    (- (mean vals) (standard-error-of-the-mean vals)))
                       #:fun-ymax (lambda (vals)
                                    (+ (mean vals) (standard-error-of-the-mean vals))))"
  
    (make-layer 'errorbar
                data
                mapping
                `((stat . ,stat)
                  (fun-ymin . ,fun-ymin)
                  (fun-ymax . ,fun-ymax)
                  (color . ,color)
                  (width . ,width)
                  (cap-width . ,cap-width))
                (if (eq? stat 'identity) #f 'stat-summary)))

;;; ------------------------------------------------------------------------
;;; layer-pointrange: Point with error bar
;;; ------------------------------------------------------------------------
  
  (define (layer-pointrange #!key (data #f) (mapping (aes))
                            (stat 'identity) (fun-ymin #f) (fun-ymax #f) (fun-y #f)
                            (color "black") (point-size 4) (line-width 2))
    "Create point + error bar layer
   
     Combines a central point with vertical error bars.
     Similar to layer-errorbar but includes the point marker.
   
     Statistical parameters:
       stat:      'identity (use y/ymin/ymax from data) or 
                  'summary (compute from raw values)
       fun-y:     Function to compute center point (default: mean)
       fun-ymin:  Function to compute lower bound (default: mean - SE)
       fun-ymax:  Function to compute upper bound (default: mean + SE)
   
     Aesthetic parameters:
       color:      Color for both point and line
       point-size: Size of center point
       line-width: Width of error bar line
   
     Required aesthetics:
       stat='identity: x, y, ymin, ymax
       stat='summary: x, y (raw values)
   
     Example:
       (layer-pointrange #:mapping (aes #:x 'treatment #:y 'response)
                         #:stat 'summary)"
  
    (make-layer 'pointrange
                data
                mapping
                `((stat . ,stat)
                  (fun-y . ,fun-y)
                  (fun-ymin . ,fun-ymin)
                  (fun-ymax . ,fun-ymax)
                  (color . ,color)
                  (point-size . ,point-size)
                  (line-width . ,line-width))
                (if (eq? stat 'identity) #f 'stat-summary)))
  
;;; ------------------------------------------------------------------------
;;; layer-linerange: Simple line ranges
;;; ------------------------------------------------------------------------
  
  (define (layer-linerange #!key (data #f) (mapping (aes))
                           (stat 'identity) (fun-ymin #f) (fun-ymax #f)
                           (color "black") (width 2))
    "Create line range layer (like error bars without caps)
   
     Statistical parameters:
       stat:      'identity or 'summary
       fun-ymin:  Function to compute lower bound
       fun-ymax:  Function to compute upper bound
   
     Aesthetic parameters:
       color: Line color
       width: Line width
   
     Required aesthetics:
       Vertical: x, ymin, ymax
       Horizontal: y, xmin, xmax
   
     Example:
       (layer-linerange #:mapping (aes #:x 'time #:ymin 'lower #:ymax 'upper))"
  
    (make-layer 'linerange
                data
                mapping
                `((stat . ,stat)
                  (fun-ymin . ,fun-ymin)
                  (fun-ymax . ,fun-ymax)
                  (color . ,color)
                  (width . ,width))
                (if (eq? stat 'identity) #f 'stat-summary)))
  
;;; ------------------------------------------------------------------------
;;; layer-crossbar: Crossbar (simplified boxplot bar)
;;; ------------------------------------------------------------------------
  
  (define (layer-crossbar #!key (data #f) (mapping (aes))
                          (stat 'identity) (fun-y #f) (fun-ymin #f) (fun-ymax #f)
                          (fill "white") (color "black") (width 0.5))
    "Create crossbar layer (short horizontal bar with center line)
   
     Statistical parameters:
       stat:      'identity or 'summary
       fun-y:     Function to compute center (default: mean)
       fun-ymin:  Function to compute lower bound
       fun-ymax:  Function to compute upper bound
   
     Aesthetic parameters:
       fill:  Fill color for bar
       color: Edge color
       width: Bar width relative to group spacing
   
     Required aesthetics:
       stat='identity: x, y, ymin, ymax
       stat='summary: x, y (raw values)
   
     Example:
       (layer-crossbar #:mapping (aes #:x 'group #:y 'mean 
                                    #:ymin 'lower #:ymax 'upper))"
  
    (make-layer 'crossbar
                data
                mapping
                `((stat . ,stat)
                  (fun-y . ,fun-y)
                  (fun-ymin . ,fun-ymin)
                  (fun-ymax . ,fun-ymax)
                  (fill . ,fill)
                  (color . ,color)
                  (width . ,width))
                (if (eq? stat 'identity) #f 'stat-summary)))
  
;;; ------------------------------------------------------------------------
;;; layer-col: Column chart with statistical summary
;;; ------------------------------------------------------------------------

  (define (layer-col #!key (data #f) (mapping (aes))
                     (fun #f) (position 'stack)
                     (fill "steelblue") (color "black") (width 0.5))
    "Create column chart with automatic grouping and summary statistics
   
     Similar to layer-bar but with statistical preprocessing.
     Automatically computes summary statistics (typically mean) for each group.
   
     Statistical parameters:
       fun:      Summary function (default: mean)
       position: How to handle multiple groups ('stack, 'dodge, 'fill)
   
     Aesthetic parameters:
       fill:  Fill color for bars
       color: Edge color
       width: Bar width relative to group spacing
   
     Required aesthetics: x (group), y (values)
     Optional aesthetics: fill (for grouping/coloring)
   
     Example:
       (layer-col #:mapping (aes #:x 'treatment #:y 'response)
                  #:fill \"steelblue\")
   
     Example with grouping:
       (layer-col #:mapping (aes #:x 'treatment #:y 'response #:fill 'gender)
                  #:position 'dodge)"
  
    (make-layer 'col
                data
                mapping
                `((fun . ,(or fun mean))
                  (position . ,position)
                  (fill . ,fill)
                  (color . ,color)
                  (width . ,width))
                'stat-summary))


(define (layer-smooth #!key (data #f) (mapping (aes))
                      (method 'loess) (se #t) (level 0.95)
                      (color "blue") (fill "lightblue") (alpha 0.4))
  "Create smoothed line layer with confidence interval
   
   Placeholder for future implementation.
   Requires statistical smoothing functions (loess, lm, etc.)
   
   Statistical parameters:
     method: Smoothing method ('loess, 'lm, 'gam)
     se:     Draw confidence interval (#t or #f)
     level:  Confidence level (default 0.95)
   
   Aesthetic parameters:
     color: Line color
     fill:  Confidence interval fill
     alpha: Transparency for confidence interval
   
   Required aesthetics: x, y"
  
  (error "layer-smooth: Not yet implemented - requires smoothing algorithms"))
  
  ;;; ========================================================================
  ;;; Scale Specifications
  ;;; ========================================================================

  (define (scale-x-continuous #!key (name #f) (limits #f) (breaks #f)
                              (labels #f) (trans #f) (expand #f))
    "Continuous x-axis scale specification"
    `(scale-x (type . continuous)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())
              ,@(if trans `((trans . ,trans)) '())
              ,@(if expand `((expand . ,expand)) '())))

  (define (scale-y-continuous #!key (name #f) (limits #f) (breaks #f)
                              (labels #f) (trans #f) (expand #f))
    "Continuous y-axis scale specification"
    `(scale-y (type . continuous)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())
              ,@(if trans `((trans . ,trans)) '())
              ,@(if expand `((expand . ,expand)) '())))

  (define (scale-x-discrete #!key (name #f) (limits #f) (breaks #f)
                            (labels #f) (expand #f))
    "Discrete x-axis scale specification"
    `(scale-x (type . discrete)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())
              ,@(if expand `((expand . ,expand)) '())))

  (define (scale-y-discrete #!key (name #f) (limits #f) (breaks #f)
                            (labels #f) (expand #f))
    "Discrete y-axis scale specification"
    `(scale-y (type . discrete)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())
              ,@(if expand `((expand . ,expand)) '())))

  (define (scale-x-log #!key (name #f) (limits #f) (breaks #f)
                       (labels #f) (base 10))
    "Logarithmic x-axis scale"
    `(scale-x (type . log)
              (base . ,base)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())))

  (define (scale-y-log #!key (name #f) (limits #f) (breaks #f)
                       (labels #f) (base 10))
    "Logarithmic y-axis scale"
    `(scale-y (type . log)
              (base . ,base)
              ,@(if name `((name . ,name)) '())
              ,@(if limits `((limits . ,limits)) '())
              ,@(if breaks `((breaks . ,breaks)) '())
              ,@(if labels `((labels . ,labels)) '())))

  (define (scale-color-manual #!key (name #f) (values '()) (limits #f))
    "Manual color scale with specified colors"
    `(scale-color (type . manual)
                  (values . ,values)
                  ,@(if name `((name . ,name)) '())
                  ,@(if limits `((limits . ,limits)) '())))

  (define (scale-color-gradient #!key (name #f) (low "blue") (high "red")
                                 (limits #f))
    "Gradient color scale"
    `(scale-color (type . gradient)
                  (low . ,low)
                  (high . ,high)
                  ,@(if name `((name . ,name)) '())
                  ,@(if limits `((limits . ,limits)) '())))

  (define (scale-fill-manual #!key (name #f) (values '()) (limits #f))
    "Manual fill scale"
    `(scale-fill (type . manual)
                 (values . ,values)
                 ,@(if name `((name . ,name)) '())
                 ,@(if limits `((limits . ,limits)) '())))

  ;;; ========================================================================
  ;;; Coordinate Systems
  ;;; ========================================================================

  (define (coord-cartesian #!key (xlim #f) (ylim #f) (expand #t))
    "Standard Cartesian coordinate system"
    `(coord (type . cartesian)
            ,@(if xlim `((xlim . ,xlim)) '())
            ,@(if ylim `((ylim . ,ylim)) '())
            (expand . ,expand)))

  (define (coord-fixed #!key (ratio 1) (xlim #f) (ylim #f))
    "Fixed aspect ratio coordinate system"
    `(coord (type . fixed)
            (ratio . ,ratio)
            ,@(if xlim `((xlim . ,xlim)) '())
            ,@(if ylim `((ylim . ,ylim)) '())))

  (define (coord-flip)
    "Flip x and y coordinates"
    `(coord (type . flip)))

  ;;; ========================================================================
  ;;; Faceting
  ;;; ========================================================================

  (define (facet-null)
    "No faceting"
    `(facet (type . null)))

  (define (facet-wrap vars #!key (ncol #f) (nrow #f) (scales "fixed")
                      (shrink #t) (labeller #f))
    "Wrap facets into rectangular layout
     
     vars: Symbol or list of symbols for faceting variables
     ncol/nrow: Number of columns/rows
     scales: 'fixed', 'free_x', 'free_y', or 'free'
     shrink: Shrink scales to data range per facet
     labeller: Label formatting function"
    
    (let ((var-list (if (symbol? vars) (list vars) vars)))
      `(facet (type . wrap)
              (vars . ,var-list)
              ,@(if ncol `((ncol . ,ncol)) '())
              ,@(if nrow `((nrow . ,nrow)) '())
              (scales . ,scales)
              (shrink . ,shrink)
              ,@(if labeller `((labeller . ,labeller)) '()))))

  (define (facet-grid row-vars col-vars #!key (scales "fixed") (space "fixed")
                       (shrink #t) (labeller #f))
    "Create grid of facets
     
     row-vars: Variables for row panels (or #f)
     col-vars: Variables for column panels (or #f)
     scales: 'fixed', 'free_x', 'free_y', or 'free'
     space: 'fixed' or 'free' (vary panel sizes with data)
     shrink: Shrink scales to data range
     labeller: Label formatting function"
    
    `(facet (type . grid)
            (rows . ,(if row-vars 
                        (if (symbol? row-vars) (list row-vars) row-vars)
                        '()))
            (cols . ,(if col-vars
                        (if (symbol? col-vars) (list col-vars) col-vars)
                        '()))
            (scales . ,scales)
            (space . ,space)
            (shrink . ,shrink)
            ,@(if labeller `((labeller . ,labeller)) '())))

  ;;; ========================================================================
  ;;; Themes
  ;;; ========================================================================

  (define (theme-minimal #!key (base-size 12) (base-family "sans"))
    "Minimal theme with light background"
    `(theme (name . minimal)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . "gray90")
                   (grid-minor . #f)
                   (border . #f))
            (axis (line . "gray50"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . #f))))

  (define (theme-classic #!key (base-size 12) (base-family "serif"))
    "Classic theme with borders, no grid"
    `(theme (name . classic)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . #f)
                   (grid-minor . #f)
                   (border . "black"))
            (axis (line . "black"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . "white"))))

  (define (theme-bw #!key (base-size 12))
    "Black and white theme"
    `(theme (name . bw)
            (base-size . ,base-size)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . "gray80")
                   (grid-minor . "gray90")
                   (border . "black"))
            (axis (line . "black"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))))

  (define (theme-void #!key (base-size 12))
    "Completely void theme (no axes, grid, etc.)"
    `(theme (name . void)
            (base-size . ,base-size)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . #f)
                   (grid-minor . #f)
                   (border . #f))
            (axis (line . #f)
                  (text . #f)
                  (ticks . #f))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))))

  (define (theme-gray #!key (base-size 12) (base-family "sans"))
    "Gray theme - the classic ggplot2 default (gray panel, white gridlines)"
    `(theme (name . gray)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "white")
            (panel (background . "gray92")
                   (grid-major . "white")
                   (grid-minor . "white")
                   (border . #f))
            (axis (line . "gray50"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . #f))))

  (define (theme-linedraw #!key (base-size 12) (base-family "sans"))
    "Linedraw theme - white background with black gridlines and border"
    `(theme (name . linedraw)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . "black")
                   (grid-minor . "gray60")
                   (border . "black"))
            (axis (line . "black"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . "white"))))

  (define (theme-light #!key (base-size 12) (base-family "sans"))
    "Light theme - white background with light gray gridlines and border"
    `(theme (name . light)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "white")
            (panel (background . "white")
                   (grid-major . "gray80")
                   (grid-minor . "gray92")
                   (border . "gray80"))
            (axis (line . "gray50"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . #f))))

  (define (theme-dark #!key (base-size 12) (base-family "sans"))
    "Dark theme - dark gray background with lighter grid (for contrast with bright data)"
    `(theme (name . dark)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (plot-background . "gray35")
            (panel (background . "gray35")
                   (grid-major . "gray50")
                   (grid-minor . "gray40")
                   (border . #f))
            (axis (line . "gray80"))
            (geometry (bar-width . 0.7)
                      (errorbar-cap-width . 0.5)
                      (errorbar-line-width . 1.5)
                      (point-size . 3.0))
            (legend (position . right)
                    (background . #f))))

  ;;; ========================================================================
  ;;; Labels
  ;;; ========================================================================

  (define (labels #!key (x #f) (y #f) (title #f) (subtitle #f) (caption #f))
    "Set axis and plot labels"
    `(labs ,@(if x `((x . ,x)) '())
           ,@(if y `((y . ,y)) '())
           ,@(if title `((title . ,title)) '())
           ,@(if subtitle `((subtitle . ,subtitle)) '())
           ,@(if caption `((caption . ,caption)) '())))

  (define labs labels)

  (define (xlab label)
    "Set x-axis label"
    `(labs (x . ,label)))

  (define (ylab label)
    "Set y-axis label"
    `(labs (y . ,label)))

  (define (ggtitle title #!optional subtitle)
    "Set plot title and optional subtitle"
    (if subtitle
        `(labs (title . ,title) (subtitle . ,subtitle))
        `(labs (title . ,title))))

  ;;; ========================================================================
  ;;; S-expression Serialization
  ;;; ========================================================================

  (define (plot->sexp plot-spec)
    "Convert plot specification to pure s-expression (serializable)"
    `(plot
      (data . ,(plot-spec-data plot-spec))
      (default-aes . ,(aes->alist (plot-spec-default-aes plot-spec)))
      (layers . ,(map layer->sexp (plot-spec-layers plot-spec)))
      (scales . ,(plot-spec-scales plot-spec))
      (coord . ,(plot-spec-coord plot-spec))
      (facet . ,(plot-spec-facet plot-spec))
      (theme . ,(plot-spec-theme plot-spec))
      (labels . ,(plot-spec-labels plot-spec))))

  (define (layer->sexp layer)
    "Convert layer to s-expression"
    `(layer (geom . ,(layer-geom layer))
            ,@(if (layer-data layer)
                 `((data . ,(layer-data layer)))
                 '())
            (aes . ,(aes->alist (layer-aes layer)))
            (params . ,(layer-params layer))
            ,@(if (layer-stat layer)
                 `((stat . ,(layer-stat layer)))
                 '())))

  (define (aes->alist aes-map)
    "Convert aesthetic mapping to alist"
    (map (lambda (key)
           (cons key (aes-get aes-map key)))
         (aes-keys aes-map)))

  (define (sexp->plot sexp)
    "Convert s-expression back to plot specification"
    (match sexp
      (('plot ('data . data)
              ('default-aes . aes-alist)
              ('layers . layer-sexps)
              ('scales . scales)
              ('coord . coord)
              ('facet . facet)
              ('theme . theme)
              ('labels . labels))
       (make-plot-spec
        data
        (alist->aes aes-alist)
        (map sexp->layer layer-sexps)
        scales
        coord
        facet
        theme
        labels))
      (else (error "Invalid plot s-expression" sexp))))

  (define (sexp->layer sexp)
    "Convert s-expression to layer"
    (let ((geom (assq-ref sexp 'geom))
          (data (assq-ref sexp 'data))
          (aes-alist (assq-ref sexp 'aes))
          (params (assq-ref sexp 'params))
          (stat (assq-ref sexp 'stat)))
      (make-layer geom data (alist->aes aes-alist) params stat)))

  (define (alist->aes alist)
    "Convert alist to aesthetic mapping"
    (apply aes
           (apply append
                  (map (lambda (pair)
                         (list (symbol->keyword (car pair)) (cdr pair)))
                       alist))))

  (define (assq-ref alist key)
    "Safe assq reference"
    (and (pair? alist)
         (if (symbol? (car alist))
             (assq-ref (cdr alist) key)
             (let ((pair (assq key alist)))
               (if pair (cdr pair) #f)))))
  
  (define (assq-ref/dflt alist key dflt)
    "Safe assq reference with default"
    (or (assq-ref alist key) dflt))


  ;;; ========================================================================
  ;;; Plot Rendering Pipeline
  ;;; ========================================================================

  ;;; Linetype helper
  ;;; Wraps a drawer with a dash pattern matching the given linetype symbol.
  (define (with-linetype lt drawer)
    (case lt
      ((dashed) (with-dash '(10.0 5.0) 0.0 drawer))
      ((dotted)  (with-dash '(2.0 5.0)  0.0 drawer))
      (else drawer)))  ;; solid / unknown: no modification

  ;;; Top-level rendering

  (define (render-plot plot-spec backend)
    "Render a plot specification to a graphics backend.

     Pipeline (per the VGE architecture):
       1. Normalize specification (defaults, validation)
       2. Train scales from all layers
       3. compile-plot -> builds a single drawer tree (pure, no I/O)
       4. render-drawer -> emits gfx-insn values into a fresh VGE
       5. vge-render! -> interprets the VGE against the backend (I/O)

     Arguments:
       plot-spec  -- plot specification created by ggplot
       backend    -- graphics backend (make-cairo-png-backend, etc.)

     Example:
       (render-plot my-plot (make-cairo-png-backend \"out.png\" 800 600))"

    (let* ((normalized  (normalize-plot-spec plot-spec))
           (scales      (train-plot-scales normalized))
           (width       (backend/get-width  backend))
           (height      (backend/get-height backend))
           (top-drawer  (compile-plot normalized scales width height))
           (vge         (make-vge)))
      (render-drawer top-drawer vge)
      (vge-render! vge backend)))

  ;;; Build the complete drawer tree for a plot (pure — no I/O).
  ;;; -----------------------------------------------------------------------
  ;;; Layout Helpers
  ;;; -----------------------------------------------------------------------

  ;;; Reference canvas for resolution-independent font scaling.
  ;;; Theme base-size values are calibrated for this pixel size.
  ;;; All font and margin sizes are scaled proportionally when rendering
  ;;; at a different resolution, preserving visual weight.
  (define gg-reference-width  800.0)
  (define gg-reference-height 600.0)

  ;;; Compute a resolution scale factor relative to the reference canvas.
  ;;; Uses the geometric mean of dimension ratios so the scale is
  ;;; independent of aspect ratio: a 1600x1200 canvas yields scale=2.0
  ;;; regardless of which dimension dominates.
  (define (compute-resolution-scale width height)
    (sqrt (* (/ (exact->inexact width)  gg-reference-width)
             (/ (exact->inexact height) gg-reference-height))))

  ;;; Return a copy of theme with base-size scaled for (width x height).
  ;;; All downstream size computations read base-size from the theme, so
  ;;; this single substitution makes fonts, margins, and tick lengths all
  ;;; scale together with the canvas resolution.
  (define (scale-theme-for-resolution theme width height)
    (let* ((scale     (compute-resolution-scale width height))
           (bs        (exact->inexact (or (assq-ref theme 'base-size) 12.0)))
           (bs-scaled (* bs scale)))
      (cons (car theme)   ;; keep leading 'theme symbol
            (map (lambda (entry)
                   (if (and (pair? entry) (eq? (car entry) 'base-size))
                       (cons 'base-size bs-scaled)
                       entry))
                 (cdr theme)))))

  (define (compute-plot-margins theme)
    "Return (margin-left margin-right margin-bottom margin-top) scaled to base-size.
     At base-size=11 (default) the margins are 77/28/61/61 px, close to the
     previous hardcoded values of 80/30/60/60."
    (let ((bs (exact->inexact (or (assq-ref theme 'base-size) 12.0))))
      (list
        (inexact->exact (round (* bs 7.0)))   ; left   (~77 @ 11pt)
        (inexact->exact (round (* bs 2.5)))   ; right  (~28 @ 11pt)
        (inexact->exact (round (* bs 5.5)))   ; bottom (~61 @ 11pt)
        (inexact->exact (round (* bs 5.5))))));top    (~61 @ 11pt)

  (define (estimate-y-axis-label-width scale label-size label-offset tick-count)
    "Estimate pixel width from the Y axis line to the leftmost extent of the
     left-axis area, including tick labels and the rotated axis title.

     Mirrors the layout in axis-drawer (gg-guides) for the left-axis case:
       tick labels: right-aligned at -label-offset
       title center: -(label-offset + max-lbl-w + label-size)
       title font: label-size * 1.2, half-width when rotated = label-size * 0.6

     The 0.65 character-width factor matches the heuristic in axis-drawer."
    (let* ((breaks    (scale-breaks scale tick-count))
           (labels    (scale-labels scale breaks))
           (max-lbl-w (if (null? labels)
                          0
                          (inexact->exact
                           (round (* (apply max (map string-length labels))
                                     label-size 0.65)))))
           ;; Space for the rotated axis title beyond the tick labels.
           ;; title-x = -(label-offset + max-lbl-w + label-size); font = label-size*1.2
           ;; left edge of title = title-x - label-size*0.6
           (title-extra (inexact->exact (round (* label-size 1.6)))))
      (+ label-offset max-lbl-w title-extra)))

  (define (make-canvas-background-drawer width height color)
    "Return a drawer that fills the entire canvas with COLOR.
     Must be prepended to all other drawers so it sits behind everything."
    (if (and color
             (not (equal? color "none"))
             (not (equal? color "transparent")))
        (with-fill-color color
          (with-pen-color color-transparent
            (filled-rect-drawer 0.0 0.0
                                (exact->inexact width)
                                (exact->inexact height))))
        empty-drawer))

  (define (compile-plot spec scales width height)
    (let* ((raw-theme (plot-spec-theme spec))
           ;; Scale base-size so fonts/margins are resolution-independent:
           ;; a 2400x1600 render gets ~2.83x larger fonts than 800x600.
           (theme    (scale-theme-for-resolution raw-theme width height)))
      ;; Install the scaled theme back into the (already-normalized) spec
      ;; so all downstream functions that call (plot-spec-theme spec) pick
      ;; up the correctly scaled base-size without requiring signature changes.
      (plot-spec-theme-set! spec theme)
      (let* ((labels   (plot-spec-labels spec))
             (panels   (compute-facet-panels spec scales width height))
             (bg-color (or (assq-ref theme 'plot-background) "white")))
      (apply combine
        (append
          ;; Full-canvas background fill (fixes transparent PNG margins)
          (list (make-canvas-background-drawer width height bg-color))
          ;; One drawer per panel
          (map (lambda (panel)
                 (compile-panel panel theme))
               panels)
          ;; Global elements: title, subtitle, legends
          (list (compile-global-elements-drawer
                   spec scales width height theme labels)))))))

  ;;; Compile a single panel into a combined drawer.
  (define (compile-panel panel theme)
    (let* ((spec   (assq-ref panel 'spec))
           (scales (assq-ref panel 'scales))
           (bounds (assq-ref panel 'bounds))
           (layers (plot-spec-layers spec))
           (scales-with-ranges (with-scale-ranges scales bounds)))
      (combine-group
        ;; Panel background + grid
        (make-panel-background-drawer bounds theme)
        ;; Data layers (non-annotation)
        (apply combine
          (map (lambda (layer)
                 (if (annotation-layer? layer)
                     empty-drawer
                     (compile-layer layer spec scales-with-ranges)))
               layers))
        ;; Annotation layers
        (apply combine
          (map (lambda (layer)
                 (if (annotation-layer? layer)
                     (compile-annotation-layer-drawer layer scales-with-ranges panel)
                     empty-drawer))
               layers))
        ;; Axes
        (compile-panel-axes-drawer scales-with-ranges theme bounds spec)
        ;; Facet strip label (when present)
        (let ((facet-label (assq-ref panel 'facet-label)))
          (if facet-label
              (compile-panel-strip-drawer bounds facet-label theme)
              empty-drawer)))))


  ;;; ------------------------------------------------------------------------
  ;;; Phase 1: Normalization
  ;;; ------------------------------------------------------------------------

  (define (normalize-plot-spec spec)
    "Fill defaults and validate specification"
    
    ;; Validate layers have required aesthetics
    (for-each validate-layer (plot-spec-layers spec))
    
    ;; Return normalized spec
    spec)

  (define (validate-layer layer)
    "Check layer has required aesthetics for its geometry"
    ;; TODO: Implement geometry-specific validation
    #t)

  ;;; ------------------------------------------------------------------------
  ;;; Phase 2: Scale Training
  ;;; ------------------------------------------------------------------------
  ;;;; Robust fix for eventplot scale training in gg-plot.scm

  (define (train-plot-scales spec)
    "Train scales from all layers' data

     Special handling:
     - ymin/ymax aesthetics train the 'y' scale (not separate scales)
     - xmin/xmax aesthetics train the 'x' scale (not separate scales)
     - For eventplot x aesthetic, column is a collection of event-time collections;
       each sub-collection is trained individually"

    (let ((data (plot-spec-data spec))
          (layers (plot-spec-layers spec))
          (default-aes (plot-spec-default-aes spec))
          (scale-specs (plot-spec-scales spec)))

      ;; Initialize empty scale collection
      (let ((scales (make-hash-table)))

        ;; Train from each layer
        (for-each
         (lambda (layer)
           (let* ((layer-geom-name (layer-geom layer))
                  (layer-data (or (layer-data layer) data))
                  (layer-aes (aes-merge default-aes (layer-aes layer)))
                  (is-eventplot (eq? layer-geom-name 'eventplot))
                  ;; Skip reference lines - they use parameters, not data
                  (is-reference-line (memq layer-geom-name '(hline vline))))

             ;; Only train scales for data-driven geometries
             (unless is-reference-line

               ;; Train each mapped aesthetic
               (for-each
                (lambda (aes-key)
                  (let ((aes-val (aes-get layer-aes aes-key)))
                    (when (and aes-val (symbol? aes-val))
                      (let ((col-data (data-column layer-data aes-val)))

                        (unless (empty? col-data)

                          ;; Map ymin/ymax to 'y' scale, xmin/xmax to 'x' scale
                          (let ((scale-key (case aes-key
                                             ((ymin ymax) 'y)
                                             ((xmin xmax) 'x)
                                             (else aes-key))))

                            ;; For eventplot x: col-data is a collection of event-time
                            ;; collections; create a linear scale and train with each
                            ;; sub-collection.  For all other columns, detect scale type
                            ;; from the first element and train directly.
                            (if (and is-eventplot (eq? aes-key 'x))
                                (let ((scale (hash-table-ref/default
                                              scales scale-key (make-scale-linear))))
                                  (for-each-elt (lambda (subcoll)
                                                 (scale-train! scale subcoll))
                                                col-data)
                                  (hash-table-set! scales scale-key scale))
                                (let ((scale (hash-table-ref/default
                                              scales scale-key
                                              (if (number? (elt-ref col-data 0))
                                                  (make-scale-linear)
                                                  (make-scale-band)))))
                                  (scale-train! scale col-data)
                                  (hash-table-set! scales scale-key scale)))))))))
                (aes-keys layer-aes)))))
         (filter layer? layers))

        ;; Detect bar-family layers once; used by both the baseline and expansion steps.
        ;; col, bar, and histogram all draw from a zero baseline.
        (let ((has-bar? (any (lambda (l) (and (layer? l) (memq (layer-geom l) '(bar col histogram))))
                             layers)))

          ;; For bar charts, the y axis must include 0 so bars are drawn from
          ;; the baseline.  Clamp the y scale lower bound to min(0, trained-y-min).
          (when has-bar?
            (let ((y-scale (hash-table-ref/default scales 'y #f)))
              (when (and y-scale
                         (scale-domain y-scale)
                         (number? (car (scale-domain y-scale))))
                (let ((d (scale-domain y-scale)))
                  (scale-set-domain! y-scale
                                     (cons (min 0 (car d)) (cdr d)))))))

          ;; For continuous numeric x/y scales:
          ;;   1. Compute nice breaks anchored to the data extents (pre-expansion)
          ;;      so tick marks coincide with actual data min/max.
          ;;   2. Expand the domain by 5% for visual padding; but for the y scale
          ;;      in bar charts, expand the top only so bars sit on the x axis.
          ;;   3. Lock in the data-anchored breaks via scale-with-breaks.
          (for-each (lambda (scale-key)
                      (let ((scale (hash-table-ref/default scales scale-key #f)))
                        (when (and scale
                                   (scale-domain scale)
                                   (number? (car (scale-domain scale))))
                          (let* ((data-domain (scale-domain scale))
                                 (dmin (car data-domain))
                                 (dmax (cdr data-domain))
                                 (n-ticks 5)
                                 ;; 1. Nice breaks: loose labelling — ticks bracket [dmin,dmax].
                                 (nice-tick-list
                                  (if (= dmin dmax)
                                      (list dmin)
                                      (nice-breaks dmin dmax n-ticks)))
                                 (tick-min (apply min nice-tick-list))
                                 (tick-max (apply max nice-tick-list))
                                 (tick-span (- tick-max tick-min))
                                 ;; 2. Per-scale expansion: read from user scale-spec if present,
                                 ;;    then fall back to defaults.
                                 ;;    Bar-Y: no gap below baseline; small gap above tallest bar.
                                 ;;    All others: symmetric 4% padding.
                                 (spec-expand
                                  (assq-ref (get-scale-spec scale-specs scale-key) 'expand))
                                 (expand
                                  (or spec-expand
                                      (if (and (eq? scale-key 'y) has-bar?)
                                          '(0 . 0.04)
                                          '(0.04 . 0.04))))
                                 (expand-lo (if (pair? expand) (car expand) expand))
                                 (expand-hi (if (pair? expand) (cdr expand) expand))
                                 ;; 3. Domain = tick range + expansion.  Ticks remain unchanged.
                                 (final-domain
                                  (cons (- tick-min (* tick-span expand-lo))
                                        (+ tick-max (* tick-span expand-hi)))))
                            (scale-set-domain! scale final-domain)
                            (hash-table-set! scales scale-key
                                             (scale-with-breaks scale nice-tick-list))))))
                    '(x y)))

        ;; Apply scale specifications (limits, transformations, etc.)
        (for-each
         (lambda (scale-spec)
           (apply-scale-spec! scales scale-spec))
         scale-specs)
        
        ;; Convert hash table to alist
        (hash-table->alist scales))))



  
  (define (get-scale-spec scale-specs scale-key)
    "Retrieve the property alist for scale-key from scale-specs list."
    (let ((target (case scale-key
                    ((x) 'scale-x)
                    ((y) 'scale-y)
                    (else scale-key))))
      (let ((entry (find (lambda (s) (eq? (car s) target)) scale-specs)))
        (if entry (cdr entry) '()))))

  (define (limits->domain limits)
    "Convert limits specification to domain pair
   
   Accepts:
   - List: '(min max) -> (min . max)
   - Pair: (min . max) -> (min . max)
    "
  
    (cond
     ((not limits) #f)
     ((pair? limits)
      (if (null? (cdr limits))
          ;; Single element list
          (cons (car limits) (car limits))
          (if (pair? (cdr limits))
              ;; Proper list '(min max)
              (cons (car limits) (cadr limits))
              ;; Already a pair (min . max)
              limits)))
     (else #f)))
  
  (define (apply-scale-spec! scales spec)
    "Apply scale specification to trained scales (with manual and gradient scale support)"
    
    (match spec
      (('scale-color . props)
       ;; Special handling for color scales
       (let ((scale-type (assq-ref props 'type)))
         (case scale-type
           ;; Manual color scale - replace trained scale entirely
           ((manual)
            (let ((values (assq-ref props 'values))
                  (limits (assq-ref props 'limits)))
              (when values
                (let ((manual-scale (make-scale-color-manual values)))
                  (let ((existing (hash-table-ref/default scales 'color #f)))
                    (when existing
                      (let ((existing-domain (scale-domain existing)))
                        (when existing-domain
                          ;; Copy domain, don't train on it
                          (scale-set-domain! manual-scale existing-domain)))))
                  
                  ;; Apply limits if specified
                  (when limits
                    (let ((domain-pair (limits->domain limits)))
                      (when domain-pair
                        (scale-set-domain! manual-scale domain-pair))))
                  
                  (hash-table-set! scales 'color manual-scale)))))
           
           
           ;; Gradient color scale - create interpolating scale
           ((gradient)
            (let ((low (assq-ref props 'low))
                  (high (assq-ref props 'high))
                  (limits (assq-ref props 'limits))
                  (existing (hash-table-ref/default scales 'color #f)))
              
              ;; Create gradient scale using gg-scales constructor
              (let ((gradient-scale (make-scale-color-gradient 
                                     #:low (or low "blue") 
                                     #:high (or high "red"))))
                
                ;; Train on existing domain if available
                (when existing
                  (let ((existing-domain (scale-domain existing)))
                    (when existing-domain
                      (scale-set-domain! gradient-scale existing-domain))))
                
                ;; Apply limits if specified
                (when limits
                  (let ((domain-pair (limits->domain limits)))
                    (when domain-pair
                      (set! gradient-scale (scale-with-domain gradient-scale domain-pair)))))
                
                ;; Replace color scale
                (hash-table-set! scales 'color gradient-scale))))
           
           
           ;; For other types, use default handling
           (else
            (apply-scale-spec-default! scales 'color props)))))
      
      ;; Fill scale (similar to color)
      (('scale-fill . props)
       (let ((scale-type (assq-ref props 'type)))
         
         (case scale-type
           ((manual)
            (let ((values (assq-ref props 'values))
                  (limits (assq-ref props 'limits)))
              
              (when values
                (let ((manual-scale (make-scale-color-manual values)))
                  (let ((existing (hash-table-ref/default scales 'fill #f)))
                    (when existing
                      (let ((existing-domain (scale-domain existing)))
                        (scale-set-domain! manual-scale existing-domain))))
                
                  (when limits
                    (let ((domain-pair (limits->domain limits)))
                      (when domain-pair
                        (scale-set-domain! manual-scale domain-pair))))
                
                  (hash-table-set! scales 'fill manual-scale)))))
           
           ((gradient)
            (let ((low (assq-ref props 'low))
                  (high (assq-ref props 'high))
                  (limits (assq-ref props 'limits))
                  (existing (hash-table-ref/default scales 'color #f)))
              
              ;; Create gradient scale using gg-scales constructor
              (let ((gradient-scale (make-scale-color-gradient 
                                     #:low (or low "blue") 
                                     #:high (or high "red"))))
                
                ;; Train on existing domain if available
                (when existing
                  (let ((existing-domain (scale-domain existing)))
                    (when existing-domain
                      (scale-set-domain! gradient-scale existing-domain))))
                
                ;; Apply limits if specified
                (when limits
                  (let ((domain-pair (limits->domain limits)))
                    (when domain-pair
                      (set! gradient-scale (scale-with-domain gradient-scale domain-pair)))))
     
                ;; Replace color scale
                (hash-table-set! scales 'color gradient-scale))))
           
           (else
            (apply-scale-spec-default! scales 'fill props)))))
      
      ;; Other scales (x, y, etc.) - use default handling
      ((scale-name . props)
       (apply-scale-spec-default! scales scale-name props))))
  
  
  
  (define (apply-scale-spec-default! scales scale-name props)
    "Default scale specification application (for x, y, etc.)"
    (let ((scale (hash-table-ref/default scales scale-name #f)))
      (when scale
        (let ((modified-scale scale))
          
          ;; Apply limits (domain)
          (let ((limits (assq-ref props 'limits)))
            (when limits
              (let ((domain-pair (limits->domain limits)))
                (when domain-pair
                  (set! modified-scale (scale-with-domain modified-scale domain-pair))))))
          
          ;; Apply transformation
          (let ((trans (assq-ref props 'trans)))
            (when trans
              (set! modified-scale 
                    (scale-with-transform modified-scale 
                                          (case trans
                                            ((log log10) 'log10)
                                            ((log2) 'log2)
                                            ((ln) 'ln)
                                            ((sqrt) 'sqrt)
                                            (else 'identity))))))
          
          ;; Apply breaks
          (let ((breaks (assq-ref props 'breaks)))
            (when breaks
              (set! modified-scale (scale-with-breaks modified-scale breaks))))
          
          ;; Update hash table
          (hash-table-set! scales scale-name modified-scale)))))

  ;;; ------------------------------------------------------------------------
  ;;; Phase 3: Faceting
  ;;; ------------------------------------------------------------------------
  
  (define (compute-facet-panels spec scales width height)
    "Compute panels for faceting.  Margins are derived from the theme base-size
     so that all font-size-sensitive spacing scales consistently."
    (let* ((theme       (plot-spec-theme spec))
           (facet-spec  (plot-spec-facet spec))
           (margins     (compute-plot-margins theme))
           (margin-left   (first  margins))
           (margin-right  (second margins))
           (margin-bottom (third  margins))
           (margin-top    (fourth margins)))

      (match facet-spec
        (('facet ('type . 'null) . _)
         ;; No faceting - single panel
         (let ((x-min margin-left)
               (y-min margin-bottom)
               (x-max (- width margin-right))
               (y-max (- height margin-top)))
           (list (make-panel spec scales #f
                             (list x-min y-min x-max y-max)))))

        (('facet ('type . 'wrap) . props)
         (compute-wrap-panels spec scales props width height))

        (('facet ('type . 'grid) . props)
         (compute-grid-panels spec scales props width height))

        (else
         (let ((x-min margin-left)
               (y-min margin-bottom)
               (x-max (- width margin-right))
               (y-max (- height margin-top)))
           (list (make-panel spec scales #f
                             (list x-min y-min x-max y-max))))))))

  
  (define (make-panel spec scales facet-label bounds)
    "Create a panel specification"
    `((spec . ,spec)
      (scales . ,scales)
      (facet-label . ,facet-label)
      (bounds . ,bounds)))  ; Normalized (x-min y-min x-max y-max)

  (define (filter-data-by-column data col-name value)
    "Return columnar data filtered to rows where col-name equals value."
    (let* ((col-vals (data-column data col-name))
           (indices
            (let ((gen (gen-elts col-vals))
                  (i 0)
                  (acc '()))
              (let loop ()
                (let ((v (gen)))
                  (if (eof-object? v)
                      (reverse acc)
                      (begin
                        (when (equal? v value)
                          (set! acc (cons i acc)))
                        (set! i (+ i 1))
                        (loop))))))))
      (map (lambda (col-entry)
             (cons (car col-entry)
                   (map (lambda (i) (elt-ref (cdr col-entry) i))
                        indices)))
           data)))

  (define (unique-ordered coll)
    "Return list of unique values from a collection, preserving first-occurrence order."
    (reverse (reduce (lambda (v acc)
                       (if (member v acc) acc (cons v acc)))
                     '()
                     coll)))

  (define (compute-wrap-panels spec scales props width height)
    "Compute panels for facet-wrap"
    (let* ((vars        (assq-ref props 'vars))
           (ncol-prop   (assq-ref props 'ncol))
           (nrow-prop   (assq-ref props 'nrow))
           (scales-mode (or (assq-ref props 'scales) "fixed"))
           (facet-var   (car vars))
           (data        (plot-spec-data spec))
           (facet-vals  (unique-ordered (data-column data facet-var)))
           (n-panels    (length facet-vals))
           (ncols       (or ncol-prop
                            (inexact->exact (ceiling (sqrt n-panels)))))
           (nrows       (or nrow-prop
                            (inexact->exact (ceiling (/ n-panels ncols)))))
           ;; Layout constants derived from theme base-size
           (theme       (plot-spec-theme spec))
           (bs          (exact->inexact (or (assq-ref theme 'base-size) 12.0)))
           (margins     (compute-plot-margins theme))
           (margin-left   (first  margins))
           (margin-right  (second margins))
           (margin-bottom (third  margins))
           (margin-top    (inexact->exact (round (* bs 6.5)))) ; extra for strips
           (strip-height  (inexact->exact (round (* bs 1.8))))
           (v-gap         (inexact->exact (round (* bs 0.9))))
           ;; Free-scale flags
           (free-y?   (or (string=? scales-mode "free_y")
                          (string=? scales-mode "free")))
           (free-x?   (or (string=? scales-mode "free_x")
                          (string=? scales-mode "free")))
           ;; Phase 1: pre-train per-panel scales so we can measure label widths
           ;; before committing to a layout.  Each entry is (val fspec panel-scales).
           (val-data
            (map (lambda (val)
                   (let* ((fdata (filter-data-by-column data facet-var val))
                          (fspec (make-plot-spec fdata
                                                 (plot-spec-default-aes spec)
                                                 (plot-spec-layers spec)
                                                 (plot-spec-scales spec)
                                                 (plot-spec-coord spec)
                                                 (plot-spec-facet spec)
                                                 (plot-spec-theme spec)
                                                 (plot-spec-labels spec)))
                          (panel-scales
                           (if (or free-y? free-x?)
                               (let ((rt (train-plot-scales fspec)))
                                 (map (lambda (s-entry)
                                        (let ((key (car s-entry)))
                                          (cond
                                           ((and (eq? key 'y) free-y?)
                                            (or (assq 'y rt) s-entry))
                                           ((and (eq? key 'x) free-x?)
                                            (or (assq 'x rt) s-entry))
                                           (else s-entry))))
                                      scales))
                               scales)))
                     (list val fspec panel-scales)))
                 facet-vals))
           ;; Phase 2: compute h-gap wide enough for the worst-case Y-axis labels.
           ;; These constants mirror compile-panel-axes-drawer and make-axis defaults.
           (tick-lbl-sz (* bs 0.9))
           (lbl-offset  15)
           (tick-count  5)
           (max-y-lbl-w
            (apply max 0
              (map (lambda (entry)
                     (let ((ps (third entry)))
                       (let ((y-sc (assq-ref ps 'y)))
                         (if y-sc
                             (estimate-y-axis-label-width
                              y-sc tick-lbl-sz lbl-offset tick-count)
                             0))))
                   val-data)))
           (h-gap (max (inexact->exact (round (* bs 5.0)))
                       (inexact->exact (round (+ max-y-lbl-w (* bs 0.5))))))
           ;; Per-panel pixel dimensions (depend on h-gap)
           (total-pw  (- width  margin-left margin-right  (* (- ncols 1) h-gap)))
           (total-ph  (- height margin-top  margin-bottom
                         (* nrows strip-height) (* (- nrows 1) v-gap)))
           (pw        (/ total-pw ncols))
           (ph        (/ total-ph nrows)))

      ;; Phase 3: assign pixel bounds to each panel using the computed h-gap.
      (let loop ((entries val-data) (idx 0) (result '()))
        (if (null? entries)
            (reverse result)
            (let* ((entry  (car entries))
                   (val    (first  entry))
                   (fspec  (second entry))
                   (ps     (third  entry))
                   (col    (remainder idx ncols))
                   (row    (quotient  idx ncols))
                   ;; x increases left->right
                   (x-min  (exact->inexact (+ margin-left (* col (+ pw h-gap)))))
                   (x-max  (exact->inexact (+ x-min pw)))
                   ;; Row 0 is the topmost row; y increases upward in libplot
                   ;; y-max of panel in row r is below the strip for that row
                   (y-max  (exact->inexact
                            (- height margin-top strip-height
                               (* row (+ ph strip-height v-gap)))))
                   (y-min  (exact->inexact (- y-max ph)))
                   (bounds (list x-min y-min x-max y-max))
                   (label-str (if (string? val)
                                  val
                                  (format #f "~a" val))))
              (loop (cdr entries)
                    (+ idx 1)
                    (cons (make-panel fspec ps label-str bounds)
                          result)))))))
  
  (define (compute-grid-panels spec scales props width height)
    "Compute panels for facet-grid"
    (let* ((theme   (plot-spec-theme spec))
           (margins (compute-plot-margins theme))
           (margin-left   (first  margins))
           (margin-right  (second margins))
           (margin-bottom (third  margins))
           (margin-top    (fourth margins))
           (x-min margin-left)
           (y-min margin-bottom)
           (x-max (- width margin-right))
           (y-max (- height margin-top)))
      (list (make-panel spec scales #f
                        (list x-min y-min x-max y-max)))))

  ;;; ------------------------------------------------------------------------
  ;;; Phase 4: Panel Rendering
  ;;; ------------------------------------------------------------------------

  
  (define (with-scale-ranges scales panel-bounds)
    "Return new scales alist with updated ranges"
    (let ((x-min (first panel-bounds))
          (y-min (second panel-bounds))
          (x-max (third panel-bounds))
          (y-max (fourth panel-bounds)))
      
      (map (lambda (scale-entry)
             (let ((key (car scale-entry))
                   (scale (cdr scale-entry)))
               (case key
                 ((x)
                  ;; Create new x-scale with updated range
                  (cons 'x (scale-with-range scale (cons x-min x-max))))
                 ((y)
                  ;; Create new y-scale with updated range
                  (cons 'y (scale-with-range scale (cons y-min y-max))))
                 (else
                  ;; Keep other scales as-is
                  scale-entry))))
           scales)))
  
  (define (compile-panel-strip-drawer bounds facet-label theme)
    "Return a drawer for the facet strip label above a panel."
    (let* ((base-size    (exact->inexact (or (assq-ref theme 'base-size) 12.0)))
           (strip-size   (* base-size 0.9))
           (strip-height (inexact->exact (round (* base-size 1.8))))
           (x-min        (first  bounds))
           (x-max        (third  bounds))
           (y-max        (fourth bounds))
           (sy-min       (exact->inexact y-max))
           (x-center     (exact->inexact (/ (+ x-min x-max) 2)))
           (y-center     (exact->inexact (+ sy-min (/ strip-height 2)))))
      (combine
        ;; Background rectangle
        (with-pen-color "gray60"
          (with-fill-color "gray90"
            (filled-rect-drawer x-min sy-min (- x-max x-min) (exact->inexact strip-height))))
        ;; Centred label
        (with-pen-color "black"
          (with-font "sans" strip-size 'normal 'normal
            (text-drawer x-center y-center facet-label
                         #:halign halign/center
                         #:valign valign/center))))))

  ;; render-panel is superseded by compile-panel (called from compile-plot).
  ;; Kept as a no-op shim so any external call-sites produce a clear error
  ;; rather than an unbound-variable panic.
  (define (render-panel panel plotter theme)
    (error "render-panel: use compile-panel / render-plot instead"))

  (define (make-panel-background-drawer bounds theme)
    "Return a drawer for the panel background, grid, and border."
    (let* ((panel-props       (assq-ref theme 'panel))
           (background-color  (assq-ref panel-props 'background))
           (grid-major-color  (assq-ref panel-props 'grid-major))
           (border-color      (assq-ref panel-props 'border))
           (x-min  (exact->inexact (first  bounds)))
           (y-min  (exact->inexact (second bounds)))
           (x-max  (exact->inexact (third  bounds)))
           (y-max  (exact->inexact (fourth bounds)))
           (w      (- x-max x-min))
           (h      (- y-max y-min)))
      (combine-group
        ;; Background fill
        (if background-color
            (with-pen-color color-transparent
              (with-fill-color background-color
                (filled-rect-drawer x-min y-min w h)))
            empty-drawer)
        ;; Major grid lines
        (if grid-major-color
            (with-pen-color grid-major-color
              (with-line-width 0.5
                (apply combine
                  (append
                    ;; 5 horizontal grid lines
                    (let ((y-step (/ h 6)))
                      (map (lambda (i)
                             (h-line-drawer x-min x-max
                                            (+ y-min (* i y-step))))
                           '(1 2 3 4 5)))
                    ;; 5 vertical grid lines
                    (let ((x-step (/ w 6)))
                      (map (lambda (i)
                             (v-line-drawer (+ x-min (* i x-step))
                                            y-min y-max))
                           '(1 2 3 4 5)))))))
            empty-drawer)
        ;; Border
        (if border-color
            (with-pen-color border-color
              (with-line-width 1.0
                (combine
                  (line-drawer x-min y-min x-max y-min)  ; bottom
                  (line-drawer x-max y-min x-max y-max)  ; right
                  (line-drawer x-max y-max x-min y-max)  ; top
                  (line-drawer x-min y-max x-min y-min)))) ; left
            empty-drawer))))

  (define (compile-layer layer spec scales)
    "Compile a single geometry layer into a drawer."
    (let* ((geom-name   (layer-geom layer))
           (layer-data  (or (layer-data layer) (plot-spec-data spec)))
           (layer-aes   (aes-merge (plot-spec-default-aes spec) (layer-aes layer)))
           (params      (layer-params layer))
           (stat        (layer-stat layer)))
      ;; Apply statistical transformation when present
      (let* ((processed-data
              (if stat
                  (case stat
                    ((stat-bin)     (stat-bin     layer-data layer-aes params))
                    ((stat-density) (stat-density layer-data layer-aes params))
                    ((stat-boxplot) (stat-boxplot layer-data layer-aes params))
                    ((stat-violin)  (stat-violin  layer-data layer-aes params))
                    ((stat-summary) (stat-summary layer-data layer-aes params))
                    (else layer-data))
                  layer-data))
             (processed-aes
              (if stat
                  (case stat
                    ((stat-bin)
                     (aes #:x 'x #:width 'width #:y 'y))
                    ((stat-density)
                     (aes #:x 'x #:y 'y))
                    ((stat-boxplot)
                     (aes #:x 'x #:ymin 'ymin #:lower 'lower
                          #:middle 'middle #:upper 'upper #:ymax 'ymax
                          #:outliers 'outliers))
                    ((stat-violin)
                     (aes #:x 'group #:y 'y-values))
                    ((stat-summary)
                     (aes #:x 'x #:y 'y #:ymin 'ymin #:ymax 'ymax))
                    (else layer-aes))
                  layer-aes)))
        ;; Dispatch to geom compiler — each returns a drawer
        (case geom-name
          ((point)     (compile-geom-point     layer-data  layer-aes  scales params))
          ((line)      (compile-geom-line      layer-data  layer-aes  scales params))
          ((bar)       (compile-geom-bar       layer-data  layer-aes  scales params))
          ((area)      (compile-geom-area      layer-data  layer-aes  scales params))
          ((rect)      (compile-geom-rect      layer-data  layer-aes  scales params))
          ((text)      (compile-geom-text      layer-data  layer-aes  scales params))
          ((segment)   (compile-geom-segment   layer-data  layer-aes  scales params))
          ((eventplot) (compile-geom-eventplot layer-data  layer-aes  scales params))
          ((hline)     (compile-geom-hline     layer-data  layer-aes  scales params))
          ((vline)     (compile-geom-vline     layer-data  layer-aes  scales params))
          ((histogram) (compile-geom-histogram processed-data processed-aes scales params))
          ((density)   (compile-geom-density   processed-data processed-aes scales params))
          ((boxplot)   (compile-geom-boxplot   processed-data processed-aes scales params))
          ((violin)    (compile-geom-violin    processed-data processed-aes scales params))
          ((errorbar)  (compile-geom-errorbar  processed-data processed-aes scales params))
          ((pointrange)(compile-geom-pointrange processed-data processed-aes scales params))
          ((linerange) (compile-geom-linerange  processed-data processed-aes scales params))
          ((crossbar)  (compile-geom-crossbar   processed-data processed-aes scales params))
          ((col)       (compile-geom-bar        processed-data processed-aes scales params))
          (else (error "compile-layer: unknown geometry" geom-name))))))

  (define (compile-geom-point data aes scales params)
    "Compile point geometry into a drawer."
    (let ((size  (or (assq-ref params 'size)  4))
          (shape (or (assq-ref params 'shape) 'circle))
          (alpha (or (assq-ref params 'alpha) 1.0)))
      (let-values (((drawer _) (geom-point data aes
                                           #:scales scales
                                           #:size size
                                           #:shape shape
                                           #:alpha alpha)))
        drawer)))

  (define (compile-geom-line data aes scales params)
    "Compile line geometry into a drawer."
    (let ((width (or (assq-ref params 'width) 2))
          (style (or (assq-ref params 'style) 'solid)))
      (let-values (((drawer _) (geom-line data aes
                                          #:scales scales
                                          #:width width
                                          #:style style)))
        drawer)))

  (define (compile-geom-bar data aes scales params)
    "Compile bar geometry into a drawer."
    (let ((fill  (or (assq-ref params 'fill)  "steelblue"))
          (width (or (assq-ref params 'width) 0.7)))
      (let-values (((drawer _) (geom-bar data aes
                                         #:scales scales
                                         #:fill fill
                                         #:width width)))
        drawer)))

  (define (compile-geom-area data aes scales params)
    "Compile area geometry into a drawer."
    (let ((fill  (or (assq-ref params 'fill)  "gray"))
          (alpha (or (assq-ref params 'alpha) 0.5)))
      (let-values (((drawer _) (geom-area data aes
                                          #:scales scales
                                          #:fill fill
                                          #:alpha alpha)))
        drawer)))

  (define (compile-geom-rect data aes scales params)
    "Compile rectangle geometry into a drawer.
     Two modes: static (coords in params) or data-mapped (via aesthetics)."
    (let ((xmin-param (assq-ref params 'xmin))
          (xmax-param (assq-ref params 'xmax))
          (ymin-param (assq-ref params 'ymin))
          (ymax-param (assq-ref params 'ymax)))
      (if (and xmin-param xmax-param ymin-param ymax-param)
          ;; Static: build a single-row dataset from params
          (let* ((dummy-data `((xmin . (,xmin-param))
                               (xmax . (,xmax-param))
                               (ymin . (,ymin-param))
                               (ymax . (,ymax-param))))
                 (dummy-aes  (make-aes #:xmin 'xmin #:xmax 'xmax
                                       #:ymin 'ymin #:ymax 'ymax))
                 (fill-param  (assq-ref params 'fill))
                 (alpha-param (assq-ref params 'alpha))
                 (geom-args   (append
                               (list dummy-data dummy-aes #:scales scales)
                               (if fill-param  (list #:fill  fill-param)  '())
                               (if alpha-param (list #:alpha alpha-param) '()))))
            (let-values (((drawer _) (apply geom-rect geom-args)))
              drawer))
          ;; Data-mapped mode
          (let-values (((drawer _) (geom-rect data aes #:scales scales)))
            drawer))))

  (define (compile-geom-text data aes scales params)
    "Compile text geometry into a drawer."
    (let ((size  (or (assq-ref params 'size)  10.0))
          (color (or (assq-ref params 'color) "black")))
      (let-values (((drawer _) (geom-text data aes
                                          #:scales scales
                                          #:size size
                                          #:color color)))
        drawer)))

  (define (compile-geom-segment data aes scales params)
    "Compile segment geometry into a drawer."
    (let ((width (or (assq-ref params 'width) 1))
          (color (or (assq-ref params 'color) "black")))
      (let-values (((drawer _) (geom-segment data aes
                                             #:scales scales
                                             #:color color
                                             #:width width)))
        drawer)))

  (define (compile-geom-eventplot data aes scales params)
    "Compile eventplot geometry into a drawer."
    (let ((line-length (or (assq-ref params 'line-length) 2.8))
          (color       (or (assq-ref params 'color) "black"))
          (width       (or (assq-ref params 'width) 4)))
      (let-values (((drawer _) (geom-eventplot data aes
                                               #:scales scales
                                               #:line-length line-length
                                               #:color color
                                               #:width width)))
        drawer)))

  ;;; hline / vline

  (define (compile-geom-hline data aes scales params)
    "Compile horizontal reference line into a drawer."
    (let* ((y-scale    (assq-ref scales 'y))
           (x-scale    (assq-ref scales 'x))
           (yintercept (assq-ref params 'yintercept))
           (color      (or (assq-ref params 'color) "black"))
           (width      (or (assq-ref params 'width) 1))
           (linetype   (or (assq-ref params 'linetype) 'solid))
           (y-pos      (scale-map y-scale yintercept))
           (x-range    (scale-range x-scale)))
      (with-linetype linetype
        (with-pen-color color
          (with-line-width (exact->inexact width)
            (h-line-drawer (car x-range) (cdr x-range) y-pos))))))

  (define (compile-geom-vline data aes scales params)
    "Compile vertical reference line into a drawer."
    (let* ((x-scale    (assq-ref scales 'x))
           (y-scale    (assq-ref scales 'y))
           (xintercept (assq-ref params 'xintercept))
           (color      (or (assq-ref params 'color) "black"))
           (width      (or (assq-ref params 'width) 1))
           (linetype   (or (assq-ref params 'linetype) 'solid))
           (x-pos      (scale-map x-scale xintercept))
           (y-range    (scale-range y-scale)))
      (with-linetype linetype
        (with-pen-color color
          (with-line-width (exact->inexact width)
            (v-line-drawer x-pos (car y-range) (cdr y-range)))))))

  ;;; Statistical geometry compilers
  ;;; These simply call the corresponding geom-* function and extract the drawer.

  (define (compile-geom-histogram data aes scales params)
    (let ((fill  (or (assq-ref params 'fill)  "steelblue"))
          (color (or (assq-ref params 'color) "black")))
      (let-values (((drawer _) (geom-histogram data aes
                                               #:scales scales
                                               #:fill fill
                                               #:color color)))
        drawer)))

  (define (compile-geom-density data aes scales params)
    (let ((fill     (or (assq-ref params 'fill)      "lightblue"))
          (alpha    (or (assq-ref params 'alpha)     0.5))
          (color    (or (assq-ref params 'color)     "navy"))
          (geom-type (or (assq-ref params 'geom-type) 'area)))
      (let-values (((drawer _) (geom-density data aes
                                             #:scales scales
                                             #:fill fill
                                             #:alpha alpha
                                             #:color color
                                             #:geom-type geom-type)))
        drawer)))

  (define (compile-geom-boxplot data aes scales params)
    (let ((fill           (or (assq-ref params 'fill)           "white"))
          (color          (or (assq-ref params 'color)          "black"))
          (outlier-color  (or (assq-ref params 'outlier-color)  "red"))
          (outlier-size   (or (assq-ref params 'outlier-size)   3))
          (width          (or (assq-ref params 'width)          0.6)))
      (let-values (((drawer _) (geom-boxplot data aes
                                             #:scales scales
                                             #:fill fill
                                             #:color color
                                             #:outlier-color outlier-color
                                             #:outlier-size outlier-size
                                             #:width width)))
        drawer)))

  (define (compile-geom-violin data aes scales params)
    (let ((fill        (or (assq-ref params 'fill)        "lightgray"))
          (color       (or (assq-ref params 'color)       "black"))
          (scale-width (or (assq-ref params 'scale-width) #t)))
      (let-values (((drawer _) (geom-violin data aes
                                            #:scales scales
                                            #:fill fill
                                            #:color color
                                            #:scale-width scale-width)))
        drawer)))

  (define (compile-geom-errorbar data aes scales params)
    (let ((color     (or (assq-ref params 'color)     "black"))
          (width     (or (assq-ref params 'width)     1.5))
          (cap-width (or (assq-ref params 'cap-width) 0.5)))
      (let-values (((drawer _) (geom-errorbar data aes
                                              #:scales scales
                                              #:color color
                                              #:width width
                                              #:cap-width cap-width)))
        drawer)))

  (define (compile-geom-pointrange data aes scales params)
    (let ((color      (or (assq-ref params 'color)      "black"))
          (point-size (or (assq-ref params 'point-size) 4))
          (line-width (or (assq-ref params 'line-width) 2)))
      (let-values (((drawer _) (geom-pointrange data aes
                                                #:scales scales
                                                #:color color
                                                #:point-size point-size
                                                #:line-width line-width)))
        drawer)))

  (define (compile-geom-linerange data aes scales params)
    (let ((color (or (assq-ref params 'color) "black"))
          (width (or (assq-ref params 'width) 2)))
      (let-values (((drawer _) (geom-linerange data aes
                                               #:scales scales
                                               #:color color
                                               #:width width)))
        drawer)))

  (define (compile-geom-crossbar data aes scales params)
    (let ((fill  (or (assq-ref params 'fill)  "white"))
          (color (or (assq-ref params 'color) "black"))
          (width (or (assq-ref params 'width) 0.5)))
      (let-values (((drawer _) (geom-crossbar data aes
                                              #:scales scales
                                              #:fill fill
                                              #:color color
                                              #:width width)))
        drawer)))

  ;;; ── Axes ─────────────────────────────────────────────────────────────────

  (define (get-scale-label scale-specs scale-key)
    "Extract label (name) from scale specification"
    (let ((scale-spec (find (lambda (s) (eq? (car s) scale-key)) scale-specs)))
      (if scale-spec
          (or (assq-ref (cdr scale-spec) 'name) "")
          "")))

  (define (compile-panel-axes-drawer scales theme bounds spec)
    "Return a drawer for panel axes (x and y)."
    (let* ((x-scale      (assq-ref scales 'x))
           (y-scale      (assq-ref scales 'y))
           (x-min        (first  bounds))
           (y-min        (second bounds))
           (scale-specs  (plot-spec-scales spec))
           (base-size    (exact->inexact (or (assq-ref theme 'base-size) 12.0)))
           (tick-lbl-sz  (* base-size 0.9)))
      (combine
        (if x-scale
            (let* ((x-label (get-scale-label scale-specs 'scale-x))
                   (x-axis  (make-axis-bottom x-scale
                                              #:label x-label
                                              #:tick-count 5
                                              #:label-size tick-lbl-sz)))
              (with-translate 0 y-min (axis-drawer x-axis)))
            empty-drawer)
        (if y-scale
            (let* ((y-label (get-scale-label scale-specs 'scale-y))
                   (y-axis  (make-axis-left y-scale
                                            #:label y-label
                                            #:tick-count 5
                                            #:label-size tick-lbl-sz)))
              (with-translate x-min 0 (axis-drawer y-axis)))
            empty-drawer))))


  ;;; ========================================================================
  ;;; Annotation Layer Constructors
  ;;; ========================================================================
  
  (define (layer-annotate-text text x y
                               #!key (size 10.0) (color "black")
                                    (hjust 0.5) (vjust 0.5)
                                    (angle 0))
    "Create annotation text layer at data coordinates
     
     Example:
       (layer-annotate-text \"Stimulus onset\" 20 0.8
                            #:color \"red\" #:size 12)"
    `(layer annotation
            (geom . text-annotation)
            (spec . ,(annotate-text text x y
                                    #:size size
                                    #:color color
                                    #:hjust hjust
                                    #:vjust vjust
                                    #:angle angle))))
  
  (define (layer-annotate-rect xmin ymin xmax ymax
                               #!key (fill "gray") (alpha 0.3)
                                    (color "none") (line-width 0))
    "Create rectangular annotation layer
     
     Example:
       (layer-annotate-rect 20 0 50 1
                            #:fill \"yellow\" #:alpha 0.2)"
    `(layer annotation
            (geom . rect-annotation)
            (spec . ,(annotate-rect xmin ymin xmax ymax
                                    #:fill fill
                                    #:alpha alpha
                                    #:color color
                                    #:line-width line-width))))
  
  (define (layer-annotate-segment x y xend yend
                                  #!key (color "black")
                                       (line-width 1)
                                       (linetype 'solid)
                                       (arrow? #f))
    "Create line segment annotation layer
     
     Example:
       (layer-annotate-segment 0 0.5 100 0.5
                               #:color \"red\" #:line-width 2)"
    `(layer annotation
            (geom . segment-annotation)
            (spec . ,(annotate-segment x y xend yend
                                       #:color color
                                       #:line-width line-width
                                       #:linetype linetype
                                       #:arrow? arrow?))))
  
  (define (layer-annotate-arrow x y xend yend
                                #!key (color "black")
                                     (line-width 1)
                                     (arrow-length 10)
                                     (arrow-angle 30))
    "Create arrow annotation layer
     
     Example:
       (layer-annotate-arrow 10 0.2 50 0.8
                             #:color \"blue\" #:arrow-length 15)"
    `(layer annotation
            (geom . arrow-annotation)
            (spec . ,(annotate-arrow x y xend yend
                                     #:color color
                                     #:line-width line-width
                                     #:arrow-length arrow-length
                                     #:arrow-angle arrow-angle))))


  ;;; ========================================================================
  ;;; Convenience Annotation Functions
  ;;; ========================================================================
  
  (define (annotate-text-layer . args)
    "Alias for layer-annotate-text for consistency with ggplot2"
    (apply layer-annotate-text args))
  
  (define (annotate-rect-layer . args)
    "Alias for layer-annotate-rect for consistency with ggplot2"
    (apply layer-annotate-rect args))
  
  (define (annotate-vline xintercept
                          #!key (color "black") (line-width 1)
                               (linetype 'dashed))
    "Create vertical reference line annotation
     
     Example:
       (annotate-vline 20 #:color \"red\" #:linetype 'dashed)"
    `(layer annotation
            (geom . vline-annotation)
            (spec . (annotation vline
                                (xintercept . ,xintercept)
                                (color . ,color)
                                (line-width . ,line-width)
                                (linetype . ,linetype)))))
  
  (define (annotate-hline yintercept
                          #!key (color "black") (line-width 1)
                               (linetype 'dashed))
    "Create horizontal reference line annotation
     
     Example:
       (annotate-hline 0 #:color \"gray\" #:linetype 'dotted)"
    `(layer annotation
            (geom . hline-annotation)
            (spec . (annotation hline
                                (yintercept . ,yintercept)
                                (color . ,color)
                                (line-width . ,line-width)
                                (linetype . ,linetype)))))
  
  (define (annotate-abline slope intercept
                           #!key (color "black") (line-width 1)
                                (linetype 'solid))
    "Create diagonal reference line (y = slope*x + intercept)
     
     Example:
       (annotate-abline 1.0 0.0 #:color \"blue\")"
    `(layer annotation
            (geom . abline-annotation)
            (spec . (annotation abline
                                (slope . ,slope)
                                (intercept . ,intercept)
                                (color . ,color)
                                (line-width . ,line-width)
                                (linetype . ,linetype)))))

  ;;; ========================================================================
  ;;; Enhanced Legend Positioning
  ;;; ========================================================================
  
  (define (theme-legend-position position)
    "Set legend position
     
     position: 'right (default), 'left, 'top, 'bottom, 'none,
               or (x . y) for manual positioning
     
     Returns: theme modification spec"
    `(theme-mod (legend.position . ,position)))
  
  (define (theme-legend-inside position)
    "Position legend inside plot panel
     
     position: 'top-left, 'top-right, 'bottom-left, 'bottom-right, 'center
     
     Returns: theme modification spec"
    `(theme-mod (legend.position . inside)
                (legend.inside-position . ,position)))
  
  (define (theme-legend-outside position)
    "Position legend outside plot panel (in margins)
     
     position: 'right, 'left, 'top, 'bottom
     
     Returns: theme modification spec"
    `(theme-mod (legend.position . outside)
                (legend.outside-position . ,position)))

  ;;; ========================================================================
  ;;; Collision-Avoiding Text Geometries
  ;;; ========================================================================
  
  (define (geom-text-repel data mapping
                           #!key (scales '())
                                (size 10.0)
                                (color "black")
                                (max-iterations 50)
                                (avoid-overlap? #t))
    "Render text labels with automatic collision avoidance
     
     Uses force-directed algorithm to prevent label overlap
     
     Required aesthetics: x, y, label
     
     Example:
       (geom-text-repel data (aes #:x 'time #:y 'value #:label 'name)
                        #:max-iterations 100)"
    
    ;; Extract data columns
    (let* ((x-col (aes-get mapping 'x))
           (y-col (aes-get mapping 'y))
           (label-col (aes-get mapping 'label))
           (x-vals (data-column data x-col))
           (y-vals (data-column data y-col))
           (labels (data-column data label-col))
           ;; Build label specs for each data point
           (label-specs
            (map (lambda (x y label)
                   (list label x y #:size size))
                 x-vals y-vals labels)))
      
      ;; Apply collision avoidance
      (label-with-collision-avoidance label-specs
                                      #:avoid-overlap? avoid-overlap?
                                      #:max-iterations max-iterations)))
  
  (define (geom-label-repel data mapping
                            #!key (scales '())
                                 (size 10.0)
                                 (fill "white")
                                 (color "black")
                                 (max-iterations 50))
    "Render text labels with boxes and collision avoidance
     
     Similar to geom-text-repel but with background boxes
     
     Required aesthetics: x, y, label
     
     Example:
       (geom-label-repel data (aes #:x 'pc1 #:y 'pc2 #:label 'sample_id)
                         #:fill \"lightyellow\")"
    
    ;; For now, delegate to geom-text-repel
    ;; TODO: Add background rectangles
    (geom-text-repel data mapping
                     #:scales scales
                     #:size size
                     #:color color
                     #:max-iterations max-iterations))


  ;;; ------------------------------------------------------------------------
  ;;; Global Elements
  ;;; ------------------------------------------------------------------------

  (define (compile-global-elements-drawer spec scales width height theme labels)
    "Return a drawer for title, subtitle, and legends."
    (let ((title    (assq-ref labels 'title))
          (subtitle (assq-ref labels 'subtitle)))
      (combine
        (if title    (compile-title-drawer    title    width height theme) empty-drawer)
        (if subtitle (compile-subtitle-drawer subtitle width height theme) empty-drawer)
        (compile-legends-drawer spec scales width height theme))))

  (define (compile-title-drawer title width height theme)
    "Return a drawer for the plot title at top-centre."
    (let* ((base-size  (or (assq-ref theme 'base-size) 12.0))
           (title-size (* base-size 1.5))
           (x          (exact->inexact (/ width 2)))
           (y          (exact->inexact (- height 30))))
      (with-pen-color "black"
        (with-font "sans" title-size 'normal 'bold
          (text-drawer x y title
                       #:halign halign/center
                       #:valign valign/center)))))

  (define (compile-subtitle-drawer subtitle width height theme)
    "Return a drawer for the plot subtitle, below the title."
    (let* ((base-size     (or (assq-ref theme 'base-size) 12.0))
           (subtitle-size (* base-size 1.2))
           (x             (exact->inexact (/ width 2)))
           (y             (exact->inexact (- height 50))))
      (with-pen-color "gray30"
        (with-font "sans" subtitle-size 'italic 'normal
          (text-drawer x y subtitle
                       #:halign halign/center
                       #:valign valign/center)))))



  (define (compile-legends-drawer spec scales width height theme)
    "Return a combined drawer for all legends."
    (let* ((legend-props     (assq-ref theme 'legend))
           (legend-position  (assq-ref legend-props 'position))
           (legend-inside-pos  (assq-ref legend-props 'inside-position))
           (legend-outside-pos (assq-ref legend-props 'outside-position))
           (color-scale (assq-ref scales 'color))
           (fill-scale  (assq-ref scales 'fill))
           (legends '()))
      ;; Build legend spec list
      (let* ((legends
              (if color-scale
                  (cons `(discrete ,(or (get-scale-label (plot-spec-scales spec) 'scale-color)
                                        "Color")
                                   items 150 200)
                        legends)
                  legends))
             (legends
              (if fill-scale
                  (cons `(discrete ,(or (get-scale-label (plot-spec-scales spec) 'scale-fill)
                                        "Fill")
                                   items 150 200)
                        legends)
                  legends)))
        ;; Nothing to draw
        (if (or (null? legends) (eq? legend-position 'none))
            empty-drawer
            (let* ((panel-bounds (assq-ref (car (compute-facet-panels spec scales width height))
                                           'bounds))
                   (x-min (first  panel-bounds)) (y-min (second panel-bounds))
                   (x-max (third  panel-bounds)) (y-max (fourth panel-bounds))
                   (panel-bbox (make-bbox x-min y-min x-max y-max))
                   (plot-bbox  (make-bbox 0 0 width height))
                   (positioned-legends
                    (cond
                      ((and (eq? legend-position 'inside) legend-inside-pos)
                       (legend-position-inside legends legend-inside-pos panel-bbox))
                      ((and (eq? legend-position 'outside) legend-outside-pos)
                       (legend-position-outside legends legend-outside-pos panel-bbox plot-bbox))
                      ((pair? legend-position)
                       (list (cons (car legends) legend-position)))
                      ((symbol? legend-position)
                       (case legend-position
                         ((inside)
                          (legend-position-inside legends
                                                  (or legend-inside-pos 'top-right)
                                                  panel-bbox))
                         ((outside)
                          (legend-position-outside legends
                                                   (or legend-outside-pos 'right)
                                                   panel-bbox plot-bbox))
                         (else
                          (legend-position-outside legends legend-position
                                                   panel-bbox plot-bbox))))
                      (else
                       (legend-position-auto legends plot-bbox panel-bbox)))))
              ;; Build a drawer for each positioned legend
              (apply combine
                (map (lambda (positioned-legend idx)
                       (let* ((legend-spec (car positioned-legend))
                              (pos         (cdr positioned-legend))
                              (legend-x    (car pos))
                              (legend-y    (cdr pos))
                              (scale       (if (= idx 0) color-scale fill-scale))
                              (title       (cadr legend-spec)))
                         (if scale
                             (let* ((base-size  (exact->inexact
                                                 (or (assq-ref theme 'base-size) 12.0)))
                                    (lbl-size   (* base-size 0.9))
                                    (legend     (make-legend-discrete scale
                                                                      #:title title
                                                                      #:position (cons legend-x legend-y)
                                                                      #:label-size lbl-size))
                                    (legend-drw (legend-drawer legend)))
                               legend-drw)
                             empty-drawer)))
                     positioned-legends
                     (iota (length positioned-legends))))
              ))
        ))
    )
  
  
(define (compile-annotation-layer-drawer layer scales panel)
  "Return a drawer for an annotation layer."
  (match layer
    (('layer 'annotation params ...)
     (let ((geom         (assq-ref params 'geom))
           (spec         (assq-ref params 'spec))
           (panel-bounds (assq-ref panel 'bounds)))
       (case geom
         ((vline-annotation) (compile-vline-annotation-drawer spec scales panel-bounds))
         ((hline-annotation) (compile-hline-annotation-drawer spec scales panel-bounds))
         ((abline-annotation)(compile-abline-annotation-drawer spec scales panel-bounds))
         (else               (render-annotation spec scales)))))
    (else empty-drawer)))
  


(define (compile-vline-annotation-drawer spec scales panel-bounds)
  "Return a drawer for a vertical reference line annotation."
  (match spec
    (('annotation 'vline params ...)
     (let* ((x        (assq-ref params 'xintercept))
            (color    (assq-ref/dflt params 'color "black"))
            (lw       (assq-ref/dflt params 'line-width 1))
            (linetype (assq-ref/dflt params 'linetype 'solid))
            (x-scale  (assq-ref scales 'x))
            (y-scale  (assq-ref scales 'y))
            (y-range  (if y-scale
                          (scale-range y-scale)
                          (cons (second panel-bounds) (fourth panel-bounds)))))
       (if x-scale
           (let ((px    (exact->inexact (scale-map x-scale x)))
                 (y-min (exact->inexact (car y-range)))
                 (y-max (exact->inexact (cdr y-range))))
             (with-linetype linetype
               (with-pen-color color
                 (with-line-width (exact->inexact lw)
                   (v-line-drawer px y-min y-max)))))
           empty-drawer)))
    (else empty-drawer)))


(define (compile-hline-annotation-drawer spec scales panel-bounds)
  "Return a drawer for a horizontal reference line annotation."
  (match spec
    (('annotation 'hline params ...)
     (let* ((y        (assq-ref params 'yintercept))
            (color    (assq-ref/dflt params 'color "black"))
            (lw       (assq-ref/dflt params 'line-width 1))
            (linetype (assq-ref/dflt params 'linetype 'solid))
            (x-scale  (assq-ref scales 'x))
            (y-scale  (assq-ref scales 'y))
            (x-range  (if x-scale
                          (scale-range x-scale)
                          (cons (first panel-bounds) (third panel-bounds)))))
       (if y-scale
           (let ((py    (exact->inexact (scale-map y-scale y)))
                 (x-min (exact->inexact (car x-range)))
                 (x-max (exact->inexact (cdr x-range))))
             (with-linetype linetype
               (with-pen-color color
                 (with-line-width (exact->inexact lw)
                   (h-line-drawer x-min x-max py)))))
           empty-drawer)))
    (else empty-drawer)))


(define (compile-abline-annotation-drawer spec scales panel-bounds)
  "Return a drawer for a diagonal (slope-intercept) reference line annotation."
  (match spec
    (('annotation 'abline params ...)
     (let* ((slope     (assq-ref params 'slope))
            (intercept (assq-ref params 'intercept))
            (color     (assq-ref/dflt params 'color "black"))
            (lw        (assq-ref/dflt params 'line-width 1))
            (linetype  (assq-ref/dflt params 'linetype 'solid))
            (x-scale   (assq-ref scales 'x))
            (y-scale   (assq-ref scales 'y)))
       (if (and x-scale y-scale)
           (let* ((x-range    (scale-range x-scale))
                  (px-min     (exact->inexact (car x-range)))
                  (px-max     (exact->inexact (cdr x-range)))
                  (data-x-min (scale-inverse x-scale px-min))
                  (data-x-max (scale-inverse x-scale px-max))
                  (py-min     (exact->inexact
                               (scale-map y-scale (+ (* slope data-x-min) intercept))))
                  (py-max     (exact->inexact
                               (scale-map y-scale (+ (* slope data-x-max) intercept)))))
             (with-linetype linetype
               (with-pen-color color
                 (with-line-width (exact->inexact lw)
                   (line-drawer px-min py-min px-max py-max)))))
           empty-drawer)))
    (else empty-drawer)))


  ;;; ========================================================================
  ;;; Helper Functions
  ;;; ========================================================================

  (define (symbol->keyword sym)
    "Convert symbol to keyword"
    (string->keyword (symbol->string sym)))


  (define (annotation-layer? layer)
    "Check if layer is an annotation (not data-driven)"
    (match layer
      (('layer 'annotation . _) #t)
      (_ #f)))
  
  
  ;;; ------------------------------------------------------------------------
  ;;; Automatic Resource Management
  ;;; ------------------------------------------------------------------------

  ;;; ========================================================================
  ;;; High-Level Save / Display Functions
  ;;; ========================================================================

  (define (ggsave plot filename #!key (width 800) (height 600))
    "Save a plot to a PNG file via the Cairo backend.

     Arguments:
       plot:     plot specification from ggplot
       filename: output file path, e.g. \"plot.png\"
       width:    pixel width  (default 800)
       height:   pixel height (default 600)

     Example:
       (ggsave my-plot \"output.png\" #:width 1000 #:height 600)"
    (let ((backend (make-cairo-png-backend filename width height)))
      (render-plot plot backend)))

  (define (ggdisplay plot #!key (width 800) (height 600))
    "Save a plot to a temporary PNG and display it.

     This is a lightweight substitute for an X11-live display.
     The temporary file is written to /tmp/ggplot-display.png.

     Example:
       (ggdisplay my-plot #:width 1000 #:height 700)"
    (let ((tmpfile "/tmp/ggplot-display.png"))
      (ggsave plot tmpfile #:width width #:height height)
      (display (string-append "Plot written to " tmpfile "\n"))))
  
  (include "gg-stat.scm")
  
) ; end module
