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
   theme-minimal
   theme-classic
   theme-bw
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
          plot
          gg-primitives
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
                          (color "black") (width 2) (cap-width 0.1))
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
                     (fill "steelblue") (color "black") (width 0.8))
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

  (define (theme-minimal #!key (base-size 11) (base-family "sans"))
    "Minimal theme with light background"
    `(theme (name . minimal)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (panel (background . "white")
                   (grid-major . "gray90")
                   (grid-minor . #f)
                   (border . #f))
            (axis (line . "gray50")
                  (text-size . 9.0)
                  (title-size . 10.0))
            (legend (position . right)
                    (background . #f))))

  (define (theme-classic #!key (base-size 11) (base-family "serif"))
    "Classic theme with borders, no grid"
    `(theme (name . classic)
            (base-size . ,base-size)
            (base-family . ,base-family)
            (panel (background . "white")
                   (grid-major . #f)
                   (grid-minor . #f)
                   (border . "black"))
            (axis (line . "black")
                  (text-size . 9.0)
                  (title-size . 10.0))
            (legend (position . right)
                    (background . "white"))))

  (define (theme-bw #!key (base-size 11))
    "Black and white theme"
    `(theme (name . bw)
            (base-size . ,base-size)
            (panel (background . "white")
                   (grid-major . "gray80")
                   (grid-minor . "gray90")
                   (border . "black"))
            (axis (line . "black")
                  (text-size . 9.0)
                  (title-size . 10.0))))

  (define (theme-void #!key (base-size 11))
    "Completely void theme (no axes, grid, etc.)"
    `(theme (name . void)
            (panel (background . "white")
                   (grid-major . #f)
                   (grid-minor . #f)
                   (border . #f))
            (axis (line . #f)
                  (text . #f)
                  (ticks . #f))
            (base-size . ,base-size)
            ))

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

  (define (render-plot plot-spec plotter-ctx)
    "Render plot specification to plotter context
   
     Pipeline:
     1. Normalize specification (defaults, validation)
     2. Train scales from all layers
     3. Compute facet panels
     4. Render each panel (background, geometries, axes, guides)
     5. Render global elements (title, legends)
   
     Arguments:
       plot-spec:    Plot specification created by ggplot
       plotter-ctx:  Plotter context from make-png-plotter, make-svg-plotter, etc.
   
     Returns: void (side effect: renders plot to output)
   
     Example:
       (let ((ctx (make-png-plotter \"output.png\" 800 600)))
         (render-plot my-plot ctx)
         (delete-plotter (plotter-context-plotter ctx)))"
  
    ;; Extract plotter and dimensions from context
    (let ((plotter (plotter-context-plotter plotter-ctx))
          (width (plotter-context-width plotter-ctx))
          (height (plotter-context-height plotter-ctx)))
      
      ;; Phase 1: Normalize
      (let* ((normalized-spec (normalize-plot-spec plot-spec))
             
             ;; Phase 2: Train scales
             (trained-scales (train-plot-scales normalized-spec))
             
             ;; Phase 3: Compute facets
             (panels (compute-facet-panels normalized-spec trained-scales width height))
             
             ;; Get theme and labels
             (theme (plot-spec-theme normalized-spec))
             (labels (plot-spec-labels normalized-spec)))
        
        ;; Initialize plotter
        (openpl plotter)
        (fspace plotter 0 0 (exact->inexact width) (exact->inexact height))
        
        ;; Phase 4: Render panels
        (for-each (lambda (panel)
                    (render-panel panel plotter theme))
                  panels)
        
        ;; Phase 5: Render global elements
        (render-global-elements normalized-spec trained-scales plotter 
                                width height theme labels)
        
        (closepl plotter))))


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

  (define (flatten-if-nested data)
    "Flatten data if it's a list of lists of numbers.
     Used for eventplot spike train data."
  
    (cond
     ;; Empty list - return as-is
     ((null? data) data)
     
     ;; First element is a list - assume nested structure, flatten
     ((and (list? (car data))
           (not (null? (car data))))
      ;; Flatten all sublists
      (apply append data))
     
     ;; First element is not a list - assume flat structure, return as-is
     (else data)))
  
  

  (define (train-plot-scales spec)
    "Train scales from all layers' data
   
     Special handling:
     - ymin/ymax aesthetics train the 'y' scale (not separate scales)
     - xmin/xmax aesthetics train the 'x' scale (not separate scales)
     - For eventplot x aesthetic, flatten nested lists of event times"
  
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
                      (let* ((col-data-raw (data-column layer-data aes-val))
                             ;; For eventplot x aesthetic: always try to flatten
                             ;; The flatten-if-nested function will detect if flattening is needed
                             (col-data 
                              (if (and is-eventplot (eq? aes-key 'x))
                                  (flatten-if-nested col-data-raw)
                                  col-data-raw)))
                        
                        (unless (null? col-data)
                          
                          ;; Map ymin/ymax to 'y' scale, xmin/xmax to 'x' scale
                          (let ((scale-key (case aes-key
                                             ((ymin ymax) 'y)
                                             ((xmin xmax) 'x)
                                             (else aes-key))))
                            
                            ;; Get or create scale
                            (let ((scale (hash-table-ref/default 
                                          scales scale-key
                                          (if (number? (car col-data))
                                              (make-scale-linear)
                                              (make-scale-band)))))
                              ;; Train scale with (possibly flattened) data
                              (scale-train! scale col-data)
                              (hash-table-set! scales scale-key scale))))))))
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
    "Compute panels for faceting with proper pixel margins
   
     Margins:
     - Left: 80px (y-axis)
     - Right: 30px (or 150px if legend)
     - Bottom: 80px (x-axis)
     - Top: 80px (title/subtitle)"
  
    (let ((facet-spec (plot-spec-facet spec))
          ;; Fixed pixel margins
          ;; margin-left = 80: leaves room for y-axis tick labels (up to ~7 chars
          ;; at 10pt) plus the rotated axis title just clear of them.
          (margin-left 80)
          (margin-right 30)
          (margin-bottom 60)
          (margin-top 60))
      
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
            (let loop ((i 0) (vs col-vals) (acc '()))
              (if (null? vs)
                  (reverse acc)
                  (loop (+ i 1) (cdr vs)
                        (if (equal? (car vs) value)
                            (cons i acc)
                            acc))))))
      (map (lambda (col-entry)
             (cons (car col-entry)
                   (map (lambda (i) (list-ref (cdr col-entry) i))
                        indices)))
           data)))

  (define (unique-ordered lst)
    "Return unique values from lst, preserving first-occurrence order."
    (let loop ((remaining lst) (seen '()) (result '()))
      (if (null? remaining)
          (reverse result)
          (let ((v (car remaining)))
            (if (member v seen)
                (loop (cdr remaining) seen result)
                (loop (cdr remaining) (cons v seen) (cons v result)))))))

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
           ;; Layout constants
           (margin-left   80)   ; room for y-axis tick labels + rotated title
           (margin-right  20)
           (margin-bottom 60)
           (margin-top    70)   ; room for title + subtitle above strips
           (strip-height  20)
           (h-gap         55)  ; room for each non-first panel's y-axis labels
           (v-gap         10)
           ;; Per-panel pixel dimensions
           (total-pw  (- width  margin-left margin-right  (* (- ncols 1) h-gap)))
           (total-ph  (- height margin-top  margin-bottom
                         (* nrows strip-height) (* (- nrows 1) v-gap)))
           (pw        (/ total-pw ncols))
           (ph        (/ total-ph nrows))
           ;; Free-scale flags
           (free-y?   (or (string=? scales-mode "free_y")
                          (string=? scales-mode "free")))
           (free-x?   (or (string=? scales-mode "free_x")
                          (string=? scales-mode "free"))))

      (let loop ((vals facet-vals) (idx 0) (result '()))
        (if (null? vals)
            (reverse result)
            (let* ((val   (car vals))
                   (col   (remainder idx ncols))
                   (row   (quotient  idx ncols))
                   ;; x increases left→right
                   (x-min (exact->inexact (+ margin-left (* col (+ pw h-gap)))))
                   (x-max (exact->inexact (+ x-min pw)))
                   ;; Row 0 is the topmost row; y increases upward in libplot
                   ;; y-max of panel in row r is below the strip for that row
                   (y-max (exact->inexact
                           (- height margin-top strip-height
                              (* row (+ ph strip-height v-gap)))))
                   (y-min (exact->inexact (- y-max ph)))
                   (bounds (list x-min y-min x-max y-max))
                   ;; Filtered plot-spec for this facet level
                   (fdata (filter-data-by-column data facet-var val))
                   (fspec (make-plot-spec fdata
                                          (plot-spec-default-aes spec)
                                          (plot-spec-layers spec)
                                          (plot-spec-scales spec)
                                          (plot-spec-coord spec)
                                          (plot-spec-facet spec)
                                          (plot-spec-theme spec)
                                          (plot-spec-labels spec)))
                   ;; Retrain scales per panel for free axes
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
                        scales))
                   (label-str (if (string? val)
                                  val
                                  (format #f "~a" val))))
              (loop (cdr vals)
                    (+ idx 1)
                    (cons (make-panel fspec panel-scales label-str bounds)
                          result)))))))
  
  (define (compute-grid-panels spec scales props width height)
    "Compute panels for facet-grid"
    (let* ((margin-left 60)
          (margin-right 30)
          (margin-bottom 60)
          (margin-top 60)
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
  
  (define (render-panel-strip plotter bounds facet-label)
    "Render the facet strip label above a panel."
    (let* ((x-min       (first  bounds))
           (x-max       (third  bounds))
           (y-max       (fourth bounds))
           (strip-height 20)
           (sy-min      (exact->inexact y-max))
           (sy-max      (exact->inexact (+ y-max strip-height)))
           (x-center    (exact->inexact (/ (+ x-min x-max) 2)))
           (y-center    (exact->inexact (/ (+ sy-min sy-max) 2))))
      ;; Background rectangle
      (render-drawer
       (rectangle x-min sy-min (- x-max x-min) strip-height
                  #:fill-color "gray90"
                  #:edge-color "gray60")
       plotter)
      ;; Centred label text
      (render-drawer
       (text x-center y-center facet-label #:size 9.0 #:color "black")
       plotter)))

  (define (render-panel panel plotter theme)
    "Render a single panel"

    (let* ((spec (assq-ref panel 'spec))
           (scales (assq-ref panel 'scales))
           (bounds (assq-ref panel 'bounds))
           (layers (plot-spec-layers spec)))
      
      ;; Set scale ranges based on panel bounds
      (let ((scales-with-ranges (with-scale-ranges scales bounds)))

        ;; Set up panel coordinate system
        (savestate plotter)
      
        ;; Apply panel bounds
        (let ((x-min (first bounds))
              (y-min (second bounds))
              (x-max (third bounds))
              (y-max (fourth bounds)))
          
          ;; Render panel background
          (render-panel-background plotter theme bounds)
        
          ;; Render each layer
          (for-each
           (lambda (layer)
             (unless (annotation-layer? layer)
               (render-layer layer spec scales-with-ranges plotter)))
           layers)

          ;; Render annotation layers
          (for-each
           (lambda (layer)
             (when (annotation-layer? layer)
               (render-annotation-layer layer scales-with-ranges panel plotter)))
           layers)
          
          ;; Render axes
          (render-panel-axes scales-with-ranges plotter theme bounds spec)

          ;; Render facet strip label above panel (when present)
          (let ((facet-label (assq-ref panel 'facet-label)))
            (when facet-label
              (render-panel-strip plotter bounds facet-label)))))

      (restorestate plotter)))

  (define (render-panel-background plotter theme bounds)
    "Render panel background and grid"
    
    (let* ((panel-props (assq-ref theme 'panel))
           (background-color (assq-ref panel-props 'background))
           (grid-major-color (assq-ref panel-props 'grid-major))
           (grid-minor-color (assq-ref panel-props 'grid-minor))
           (border-color (assq-ref panel-props 'border))
           (x-min (first bounds))
           (y-min (second bounds))
           (x-max (third bounds))
           (y-max (fourth bounds)))
      
      ;; Save state before any background rendering
      (savestate plotter)
      
      ;; Background fill
      (when background-color
        (fillcolorname plotter background-color)
        (filltype plotter 1)
        (fbox plotter x-min y-min x-max y-max))
      
      ;; Restore state to clear filltype and any other settings
      (restorestate plotter)
      
      ;; Grid and border rendering in their own isolated state
      (savestate plotter)
      
      ;; Grid lines (major)
      (when grid-major-color
        (pencolorname plotter grid-major-color)
        (linewidth plotter 1)
        ;; Draw horizontal grid lines (simplified - 5 lines)
        (let ((y-step (/ (- y-max y-min) 6)))
          (do ((i 1 (+ i 1)))
              ((> i 5))
            (let ((y (+ y-min (* i y-step))))
              (fline plotter
                     (exact->inexact x-min) (exact->inexact y)
                     (exact->inexact x-max) (exact->inexact y)))))
        ;; Draw vertical grid lines
        (let ((x-step (/ (- x-max x-min) 6)))
          (do ((i 1 (+ i 1)))
              ((> i 5))
            (let ((x (+ x-min (* i x-step))))
              (fline plotter
                     (exact->inexact x) (exact->inexact y-min)
                     (exact->inexact x) (exact->inexact y-max))))))
      
      ;; Border
      (when border-color
        (pencolorname plotter border-color)
        (linewidth plotter 1)
        (fline plotter x-min y-min x-max y-min)  ; Bottom
        (fline plotter x-max y-min x-max y-max)  ; Right
        (fline plotter x-max y-max x-min y-max)  ; Top
        (fline plotter x-min y-max x-min y-min)) ; Left
      
      ;; Restore state to clear any pen/line settings
      (restorestate plotter)))

  (define (render-layer layer spec scales plotter)
    "Render a single geometry layer"
    
    (let* ((geom-name (layer-geom layer))
           (layer-data (or (layer-data layer) (plot-spec-data spec)))
           (layer-aes (aes-merge (plot-spec-default-aes spec) (layer-aes layer)))
           (params (layer-params layer))
           (stat (layer-stat layer)))
      
    ;; Apply statistical transformation if specified
    (let* ((processed-data 
            (if stat
                (case stat
                  ((stat-bin) (stat-bin layer-data layer-aes params))
                  ((stat-density) (stat-density layer-data layer-aes params))
                  ((stat-boxplot) (stat-boxplot layer-data layer-aes params))
                  ((stat-violin) (stat-violin layer-data layer-aes params))
                  ((stat-summary) (stat-summary layer-data layer-aes params))
                  (else layer-data))

                layer-data))
           ;; Create aesthetic mapping for processed data
           ;; For stat transformations, we need to update aesthetic mapping
           ;; to reference the transformed column names
           (processed-aes 
             (if stat
                 (case stat
                   ((stat-bin) 
                    ;; stat-bin produces: x (centers), width, y (counts)
                    (aes #:x 'x #:width 'width #:y 'y))
                   
                   ((stat-density)
                    ;; stat-density produces: x (points), y (density)
                    (aes #:x 'x #:y 'y))
                   
                   ((stat-boxplot)
                    ;; stat-boxplot produces: x, ymin, lower, middle, upper, ymax, outliers
                    (aes #:x 'x #:ymin 'ymin #:lower 'lower 
                         #:middle 'middle #:upper 'upper #:ymax 'ymax
                         #:outliers 'outliers))
                   
                   ((stat-violin)
                    ;; stat-violin produces: group, density-x, density-y, y-values
                    (aes #:x 'group #:y 'y-values))
                   
                   ((stat-summary)
                    ;; stat-summary produces: x, y, ymin, ymax
                    (aes #:x 'x #:y 'y #:ymin 'ymin #:ymax 'ymax))
                   
                   (else layer-aes))
                 layer-aes))

           )
      
      ;; Dispatch to geometry renderer
      (case geom-name
        ((point) (render-geom-point layer-data layer-aes scales params plotter))
        ((line) (render-geom-line layer-data layer-aes scales params plotter))
        ((bar) (render-geom-bar layer-data layer-aes scales params plotter))
        ((area) (render-geom-area layer-data layer-aes scales params plotter))
        ((rect) (render-geom-rect layer-data layer-aes scales params plotter))
        ((text) (render-geom-text layer-data layer-aes scales params plotter))
        ((segment) (render-geom-segment layer-data layer-aes scales params plotter))
        ((eventplot) (render-geom-eventplot layer-data layer-aes scales params plotter))
        ;; Reference lines (don't use data)
        ((hline) (render-geom-hline layer-data layer-aes scales params plotter))
        ((vline) (render-geom-vline layer-data layer-aes scales params plotter))

        ;; Statistical geometries
        ((histogram) 
         (render-geom-histogram processed-data processed-aes scales params plotter))
        
        ((density) 
         (render-geom-density processed-data processed-aes scales params plotter))
        
        ((boxplot) 
         (render-geom-boxplot processed-data processed-aes scales params plotter))
        
        ((violin) 
         (render-geom-violin processed-data processed-aes scales params plotter))
        
        ((errorbar) 
         (render-geom-errorbar processed-data processed-aes scales params plotter))
        
        ((pointrange)
         (render-geom-pointrange processed-data processed-aes scales params plotter))
        
        ((linerange)
         (render-geom-linerange processed-data processed-aes scales params plotter))
        
        ((crossbar)
         (render-geom-crossbar processed-data processed-aes scales params plotter))
        
        ((col)
         ;; Column chart uses summary stat, then renders as bars
         (render-geom-bar processed-data processed-aes scales params plotter))
        
        (else (error "Unknown geometry" geom-name)))))
    )

  (define (render-geom-point data aes scales params plotter)
    "Render point geometry with parameters"
    (let ((size (or (assq-ref params 'size) 4))
          (shape (or (assq-ref params 'shape) 'circle))
          (alpha (or (assq-ref params 'alpha) 1.0)))
      (let-values (((drawer _) (geom-point data aes 
                                           #:scales scales
                                           #:size size
                                           #:shape shape
                                           #:alpha alpha)))
        (render-drawer drawer plotter))))
  
  (define (render-geom-line data aes scales params plotter)
    "Render line geometry with parameters"
    (let ((width (or (assq-ref params 'width) 2))
          (style (or (assq-ref params 'style) 'solid)))
      (let-values (((drawer _) (geom-line data aes 
                                          #:scales scales
                                          #:width width
                                          #:style style)))
        (render-drawer drawer plotter))))
  
  (define (render-geom-bar data aes scales params plotter)
    "Render bar geometry with parameters"
    (let ((fill (or (assq-ref params 'fill) "steelblue"))
          (width (or (assq-ref params 'width) 0.8)))
      (let-values (((drawer _) (geom-bar data aes 
                                         #:scales scales
                                         #:fill fill
                                         #:width width)))
        (render-drawer drawer plotter))))
  
  (define (render-geom-area data aes scales params plotter)
    "Render area geometry with parameters"
    (let ((fill (or (assq-ref params 'fill) "gray"))
          (alpha (or (assq-ref params 'alpha) 0.5)))
      (let-values (((drawer _) (geom-area data aes 
                                          #:scales scales
                                          #:fill fill
                                          #:alpha alpha)))
        (render-drawer drawer plotter))))
  
  (define (render-geom-rect data aes scales params plotter)
    "Render rectangle geometry
   
     Supports two modes:
     1. Data-mapped: aesthetics map to data columns
     2. Static: coordinates provided as parameters"
  
    ;; Check if we have static coordinates in params
    (let ((xmin-param (assq-ref params 'xmin))
          (xmax-param (assq-ref params 'xmax))
          (ymin-param (assq-ref params 'ymin))
          (ymax-param (assq-ref params 'ymax)))
      
      (if (and xmin-param xmax-param ymin-param ymax-param)
          ;; Static rect mode: create dummy single-row data
          (let* ((dummy-data `((xmin . (,xmin-param))
                               (xmax . (,xmax-param))
                               (ymin . (,ymin-param))
                               (ymax . (,ymax-param))))
                 (dummy-aes (make-aes #:xmin 'xmin #:xmax 'xmax 
                                      #:ymin 'ymin #:ymax 'ymax))
                 ;; Extract visual parameters
                 (fill-param (assq-ref params 'fill))
                 (alpha-param (assq-ref params 'alpha)))
            
            ;; Build keyword arguments for geom-rect
            (let ((geom-args (append
                              (list dummy-data dummy-aes)
                              (list #:scales scales)
                              (if fill-param (list #:fill fill-param) '())
                              (if alpha-param (list #:alpha alpha-param) '()))))
              (let-values (((drawer _) (apply geom-rect geom-args)))
                (render-drawer drawer plotter))))
          
          ;; Data-mapped mode: use aesthetics to map data columns
          (let-values (((drawer _) (geom-rect data aes #:scales scales)))
            (render-drawer drawer plotter)))))
  
  (define (render-geom-text data aes scales params plotter)
    "Render text geometry"
    (let ((size (or (assq-ref params 'size) 10.0))
          (color (or (assq-ref params 'color) "black")))
      (let-values (((drawer _) (geom-text data aes 
                                          #:scales scales
                                          #:size size
                                          #:color color)))
        (render-drawer drawer plotter))))

  (define (render-geom-segment data aes scales params plotter)
    "Render segment geometry"
    (let ((width (or (assq-ref params 'width) 1))
          (color (or (assq-ref params 'color) "black")))
      (let-values (((drawer _) (geom-segment data aes
                                             #:scales scales
                                             #:color color
                                             #:width width)))
        (render-drawer drawer plotter))))

  (define (render-geom-eventplot data aes scales params plotter)
    "Render eventplot geometry"
    (let ((line-length (or (assq-ref params 'line-length) 2.8))
          (color (or (assq-ref params 'color) "black"))
          (width (or (assq-ref params 'width) 4)))
      (let-values (((drawer _) (geom-eventplot data aes #:scales scales
                                               #:line-length line-length
                                               #:color color
                                               #:width width)))
        (render-drawer drawer plotter))))

  (define (render-geom-hline data aes scales params plotter)
    "Render horizontal reference line
     
     Unlike other geometries, hline doesn't use data.
     It draws a line at a fixed y-value across the entire x-range."
    
    (let* ((y-scale (assq-ref scales 'y))
           (x-scale (assq-ref scales 'x))
           (yintercept (assq-ref params 'yintercept))
           (color (or (assq-ref params 'color) "black"))
           (width (or (assq-ref params 'width) 1))
           (linetype (or (assq-ref params 'linetype) 'solid)))
      
      ;; Get the y-position from the intercept value
      (let ((y-pos (scale-map y-scale yintercept))
            (x-range (scale-range x-scale)))
        
        ;; Draw line across full x-range
        (savestate plotter)
        (pencolorname plotter color)
        (linewidth plotter width)
        
        ;; Set line style
        (case linetype
          ((dashed) (linedash plotter (list 10 5) 0))
          ((dotted) (linedash plotter (list 2 5) 0))
          ((solid) (linedash plotter '() 0)))
        
        (fline plotter (car x-range) y-pos (cdr x-range) y-pos)
        (restorestate plotter))))

  (define (render-geom-vline data aes scales params plotter)
    "Render vertical reference line
     
     Unlike other geometries, vline doesn't use data.
     It draws a line at a fixed x-value across the entire y-range."

    (let* ((x-scale (assq-ref scales 'x))
           (y-scale (assq-ref scales 'y))
           (xintercept (assq-ref params 'xintercept))
           (color (or (assq-ref params 'color) "black"))
           (width (or (assq-ref params 'width) 1))
           (linetype (or (assq-ref params 'linetype) 'solid)))
      
      ;; Get the x-position from the intercept value
      (let ((x-pos (scale-map x-scale xintercept))
            (y-range (scale-range y-scale)))
        
        ;; Draw line across full y-range
        (savestate plotter)
        (pencolorname plotter color)
        (flinewidth plotter (exact->inexact width))
        
        ;; Set line style
        (case linetype
          ((dashed) (linedash plotter (list 10 5) 0))
          ((dotted) (linedash plotter (list 2 5) 0))
          ((solid) (linedash plotter '() 0)))
        
        (fline plotter (exact->inexact x-pos)
               (exact->inexact (car y-range))
               (exact->inexact x-pos)
               (exact->inexact (cdr y-range)))
        (restorestate plotter))))

  (define (get-scale-label scale-specs scale-key)
    "Extract label (name) from scale specification"
    (let ((scale-spec (find (lambda (s) (eq? (car s) scale-key)) scale-specs)))
      (if scale-spec
          (or (assq-ref (cdr scale-spec) 'name) "")
          "")))
  
  (define (render-panel-axes scales plotter theme bounds spec)
    "Render panel axes using gg-guides"
    
    (let ((x-scale (assq-ref scales 'x))
          (y-scale (assq-ref scales 'y))
          (x-min (first bounds))
          (y-min (second bounds))
          (scale-specs (plot-spec-scales spec)))
    
      (when x-scale
        ;; Extract x-axis label from scale specification
        (let* ((x-label (get-scale-label scale-specs 'scale-x))
               (x-axis (make-axis-bottom x-scale 
                                         #:label x-label 
                                         #:tick-count 5))
               (axis-drw (axis-drawer x-axis))
               (positioned-drw (with-translate 0 y-min axis-drw)))
          (render-drawer positioned-drw plotter)))
      
      (when y-scale
        ;; Extract y-axis label from scale specification
        (let* ((y-label (get-scale-label scale-specs 'scale-y))
               (y-axis (make-axis-left y-scale 
                                       #:label y-label 
                                       #:tick-count 5))
               (axis-drw (axis-drawer y-axis))
               (positioned-drw (with-translate x-min 0 axis-drw)))
          (render-drawer positioned-drw plotter)))))


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

  (define (render-global-elements spec scales plotter width height theme labels)
    "Render title, legends, and other global elements"
    
    ;; Render title if specified
    (let ((title (assq-ref labels 'title)))
      (when title
        (render-title title plotter width height theme)))

    ;; Render subtitle if specified
    (let ((subtitle (assq-ref labels 'subtitle)))
      (when subtitle
        (render-subtitle subtitle plotter width height theme)))
    
    ;; Render legends
    (render-legends spec scales plotter width height theme))

  (define (render-title title plotter width height theme)
    "Render plot title"
    (let* ((base-size (or (assq-ref theme 'base-size) 11.0))
           (title-size (* base-size 1.5))
           (x (/ width 2))
           (y (- height 30)))
      
      ;; Position title at top center
      (savestate plotter)
      (ffontsize plotter (exact->inexact title-size))
      (pencolorname plotter "black")
      (move plotter x y)
      (alabel plotter (HCenter) (VCenter) title)
      (restorestate plotter)))

  (define (render-subtitle subtitle plotter width height theme)
    "Render plot subtitle below title"
    (let* ((base-size (or (assq-ref theme 'base-size) 11.0))
           (subtitle-size (* base-size 1.2))
           (x (/ width 2))
           (y (- height 50)))  ; Position below title (title is at height-30)
      
      ;; Position subtitle at top center, below title
      (savestate plotter)
      (ffontsize plotter (exact->inexact subtitle-size))
      (pencolorname plotter "gray30")
      (move plotter x y)
      (alabel plotter (HCenter) (VCenter) subtitle)
      (restorestate plotter)))



  (define (render-legends spec scales plotter width height theme)
    "Render legends with positioning strategies"
    (let* ((legend-props (assq-ref theme 'legend))
           (legend-position (assq-ref legend-props 'position))
           (legend-inside-pos (assq-ref legend-props 'inside-position))
           (legend-outside-pos (assq-ref legend-props 'outside-position))
           (color-scale (assq-ref scales 'color))
           (fill-scale (assq-ref scales 'fill))
           (legends '())
           )
      ;; Collect legends to render
      (let* ((legends
              ;; Build legend specs
              (if color-scale
                  (let ((name (get-scale-label (plot-spec-scales spec) 'scale-color)))
                    (cons `(discrete ,(or name "Color") items 150 200)
                          legends))
                  legends))
             (legends
              (if fill-scale
                  (let ((name (get-scale-label (plot-spec-scales spec) 'scale-fill)))
                    (cons `(discrete ,(or name "Fill") items 150 200)
                          legends))
                  legends)))
        
        ;; Skip if no legends or position is 'none
        (unless (or (null? legends) (eq? legend-position 'none))
          
          ;; Create bounding boxes for positioning
          (let* ((panel-bounds (assq-ref (car (compute-facet-panels spec scales width height))
                                         'bounds))
                 (x-min (first panel-bounds))
                 (y-min (second panel-bounds))
                 (x-max (third panel-bounds))
                 (y-max (fourth panel-bounds))
                 (panel-bbox (make-bbox x-min y-min x-max y-max))
                 (plot-bbox (make-bbox 0 0 width height))
                 
                 ;; Determine positioning strategy
                 (positioned-legends
                  (cond
                   ;; Inside panel positioning
                   ((and (eq? legend-position 'inside) legend-inside-pos)
                    (legend-position-inside legends legend-inside-pos panel-bbox))
                   
                   ;; Outside panel positioning
                   ((and (eq? legend-position 'outside) legend-outside-pos)
                    (legend-position-outside legends legend-outside-pos panel-bbox plot-bbox))
                   
                   ;; Manual absolute position
                   ((pair? legend-position)
                    (list (cons (car legends) legend-position)))
                   
                   ;; Symbol position (right, left, top, bottom)
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
                   
                   ;; Default: auto positioning (right margin)
                   (else
                    (legend-position-auto legends plot-bbox panel-bbox)))))
          
            ;; Render each positioned legend
            (for-each
             (lambda (positioned-legend idx)
               (let* ((legend-spec (car positioned-legend))
                      (pos (cdr positioned-legend))
                      (legend-x (car pos))
                      (legend-y (cdr pos))
                      ;; Get corresponding scale
                      (scale (if (= idx 0) color-scale fill-scale))
                      (title (cadr legend-spec)))
                 
                 (when scale
                   (let* ((legend (make-legend-discrete scale
                                                        #:title title
                                                        #:position (cons legend-x legend-y)))
                          (legend-drw (legend-drawer legend)))
                     (render-drawer legend-drw plotter)))))
             positioned-legends
             (iota (length positioned-legends)))))))
    )
  
(define (render-annotation-layer layer scales panel plotter)
  "Render an annotation layer with type-specific handling
   
   Dispatches to specialized renderers for vline, hline, abline annotations"
  (match layer
    (('layer 'annotation params ...)
     (let ((geom (assq-ref params 'geom))
           (spec (assq-ref params 'spec))
           (panel-bounds (assq-ref panel 'bounds)))

       ;; Dispatch based on annotation geometry type
       (case geom
         ;; Reference lines use specialized renderers
         ((vline-annotation)
          (render-vline-annotation spec scales panel-bounds plotter))
         
         ((hline-annotation)
          (render-hline-annotation spec scales panel-bounds plotter))
         
         ((abline-annotation)
          (render-abline-annotation spec scales panel-bounds plotter))
         
         ;; General annotations use render-annotation from gg-layout
         (else
          (let ((drawer (render-annotation spec scales)))
            (render-drawer drawer plotter))))))
    (else (void))))
  


(define (render-vline-annotation spec scales panel-bounds plotter)
  "Render vertical reference line with linetype handling"
  (match spec
    (('annotation 'vline params ...)
     (let* ((x (assq-ref params 'xintercept))
            (color (assq-ref/dflt params 'color "black"))
            (lw (assq-ref/dflt params 'line-width 1))
            (linetype (assq-ref/dflt params 'linetype 'solid))
            (x-scale (assq-ref scales 'x))
            (y-scale (assq-ref scales 'y))
            ;; Use panel bounds for y-range if scale not available
            (y-range (if y-scale
                        (scale-range y-scale)
                        (cons (second panel-bounds) (fourth panel-bounds)))))

       (when x-scale  ; Only render if x-scale exists
         (let ((px (scale-map x-scale x))
               (y-min (car y-range))
               (y-max (cdr y-range)))
           ;; Save state and apply line style
           (savestate plotter)
           (pencolorname plotter color)
           (flinewidth plotter (exact->inexact lw))
           
           ;; Apply linetype
           (case linetype
             ((dashed) (linedash plotter (list 10 5) 0))
             ((dotted) (linedash plotter (list 2 5) 0))
             ((solid) (linedash plotter '() 0)))
           
           ;; Draw line
           (fline plotter 
                  (exact->inexact px) (exact->inexact y-min)
                  (exact->inexact px) (exact->inexact y-max))
           
           (restorestate plotter)))))))


(define (render-hline-annotation spec scales panel-bounds plotter)
  "Render horizontal reference line with linetype handling"
  (match spec
    (('annotation 'hline params ...)
     (let* ((y (assq-ref params 'yintercept))
            (color (assq-ref/dflt params 'color "black"))
            (lw (assq-ref/dflt params 'line-width 1))
            (linetype (assq-ref/dflt params 'linetype 'solid))
            (x-scale (assq-ref scales 'x))
            (y-scale (assq-ref scales 'y))
            ;; Use panel bounds for x-range if scale not available
            (x-range (if x-scale
                        (scale-range x-scale)
                        (cons (first panel-bounds) (third panel-bounds)))))
       
       (when y-scale  ; Only render if y-scale exists
         (let ((py (scale-map y-scale y))
               (x-min (car x-range))
               (x-max (cdr x-range)))
           
           ;; Save state and apply line style
           (savestate plotter)
           (pencolorname plotter color)
           (flinewidth plotter (exact->inexact lw))
           
           ;; Apply linetype
           (case linetype
             ((dashed) (linedash plotter (list 10 5) 0))
             ((dotted) (linedash plotter (list 2 5) 0))
             ((solid) (linedash plotter '() 0)))
           
           ;; Draw line
           (fline plotter 
                  (exact->inexact x-min) (exact->inexact py)
                  (exact->inexact x-max) (exact->inexact py))
           
           (restorestate plotter)))))))
  

(define (render-abline-annotation spec scales panel-bounds plotter)
  "Render diagonal reference line with proper coordinate transformation"
  (match spec
    (('annotation 'abline params ...)
     (let* ((slope (assq-ref params 'slope))
            (intercept (assq-ref params 'intercept))
            (color (assq-ref/dflt params 'color "black"))
            (lw (assq-ref/dflt params 'line-width 1))
            (linetype (assq-ref/dflt params 'linetype 'solid))
            (x-scale (assq-ref scales 'x))
            (y-scale (assq-ref scales 'y)))
       
       (when (and x-scale y-scale)  ; Need both scales
         (let* ((x-range (scale-range x-scale))
                (x-min (car x-range))
                (x-max (cdr x-range))
                ;; Compute endpoints in data space
                (data-x-min (scale-inverse x-scale x-min))
                (data-x-max (scale-inverse x-scale x-max))
                (data-y-min (+ (* slope data-x-min) intercept))
                (data-y-max (+ (* slope data-x-max) intercept))
                ;; Transform back to pixel space
                (py-min (scale-map y-scale data-y-min))
                (py-max (scale-map y-scale data-y-max)))
           
           ;; Save state and apply line style
           (savestate plotter)
           (pencolorname plotter color)
           (flinewidth plotter (exact->inexact lw))
           
           ;; Apply linetype
           (case linetype
             ((dashed) (linedash plotter (list 10 5) 0))
             ((dotted) (linedash plotter (list 2 5) 0))
             ((solid) (linedash plotter '() 0)))
           
           ;; Draw line
           (fline plotter 
                  (exact->inexact x-min) (exact->inexact py-min)
                  (exact->inexact x-max) (exact->inexact py-max))
           
           (restorestate plotter)))))))

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

  (define (with-plotter-context plotter-ctx thunk)
    "Execute thunk with plotter context, ensuring cleanup
   
   Example:
     (with-plotter-context (make-png-plotter \"out.png\" 800 600)
       (lambda (ctx)
         (render-plot my-plot ctx)))"
    (dynamic-wind
        (lambda () #f)
        (lambda () (thunk plotter-ctx))
        (lambda () (delete-plotter (plotter-context-plotter plotter-ctx)))))

  ;;; ------------------------------------------------------------------------ 
  ;;; High-Level Save Functions
  ;;; ------------------------------------------------------------------------

  (define (ggsave plot filename #!key (width 800) (height 600) (scale 2))
    "Save plot to PNG file with automatic resource management
   
     Arguments:
       plot:     Plot specification from ggplot
       filename: Output filename (e.g., \"myplot.png\")
       width:    Plot width in points (default: 800)
       height:   Plot height in points (default: 600)
       scale:    Resolution multiplier (default: 2)
   
     Example:
       (ggsave my-plot \"output.png\" #:width 1000 #:height 600)"
  
    (with-plotter-context
     (make-png-plotter filename width height scale)
     (lambda (ctx)
       (render-plot plot ctx))))

  (define (ggdisplay plot #!key (width 800) (height 600) (display-name ":0"))
    "Display plot in X11 window (interactive)
   
     Arguments:
       plot:         Plot specification from ggplot
       width:        Window width in pixels (default: 800)
       height:       Window height in pixels (default: 600)
       display-name: X11 display (default: \":0\")
   
     Example:
       (ggdisplay my-plot #:width 1000 #:height 700)"
  
    (with-plotter-context
     (make-x-plotter width height display-name)
     (lambda (ctx)
       (render-plot plot ctx))))
  
  (include "gg-stat.scm")
  
) ; end module
