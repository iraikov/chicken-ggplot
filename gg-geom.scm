;;;; gg-geom.scm
;;;; Grammar of Graphics - Geometries
;;;;
;;;; Data-driven geometric representations with aesthetic mapping

(module gg-geom
  (;; Core geometries
   geom-point
   geom-line
   geom-path
   geom-area
   geom-bar
   geom-rect
   geom-text
   geom-segment
   geom-hline
   geom-vline
   
   ;; Event plot
   geom-eventplot

   ;; Geometry combinators
   geom-with-ranges

   ;; Statistical geometries
   geom-histogram
   geom-density
   geom-boxplot
   geom-violin
   geom-errorbar
   geom-crossbar
   geom-linerange
   geom-pointrange
   )

  (import scheme
          (chicken base)
          (chicken condition)
          (chicken format)
          (chicken sort)
          srfi-1
          gg-primitives-vge
          gg-scales
          gg-data
          gg-aes)

  ;;; ========================================================================
  ;;; Local bridge: old keyword-arg primitive API -> new VGE drawer combinators
  ;;; ========================================================================

  (define (circle cx cy r #!key (fill-color "black") (edge-color "black"))
    (with-pen-color edge-color
      (with-fill-color fill-color
        (filled-circle-drawer cx cy r))))

  (define (polyline pts #!key (color "black") (width 1) (style 'solid))
    (with-pen-color color
      (with-line-width width
        (polyline-drawer pts))))

  (define (polygon pts #!key (fill-color "lightgray") (edge-color "none"))
    (with-pen-color edge-color
      (with-fill-color fill-color
        (filled-polygon-drawer pts))))

  (define (rectangle x y w h #!key (fill-color "lightgray") (edge-color "black"))
    (with-pen-color edge-color
      (with-fill-color fill-color
        (filled-rect-drawer x y w h))))

  (define (text x y str #!key (color "black") (size 10.0))
    (with-pen-color color
      (with-font "sans" size 'normal 'normal
        (text-drawer x y str))))

  (define (line x1 y1 x2 y2 #!key (color "black") (width 1) (style 'solid))
    (with-pen-color color
      (with-line-width width
        (line-drawer x1 y1 x2 y2))))

  ;;; ========================================================================
  ;;; Geometry Helpers
  ;;; ========================================================================

  (define (get-or-create-scale scales key default-maker)
    "Get existing scale or create new one"
    (let ((existing (assoc key scales)))
      (if existing
          (values (cdr existing) scales)
          (let ((new-scale (default-maker)))
            (values new-scale
                    (cons (cons key new-scale) scales))))))

  (define (train-scales-from-data data aesthetic-map scales)
    "Train scales based on aesthetic mappings and data"
    (let loop ((aes-keys (aes-keys aesthetic-map))
               (current-scales scales))
      (if (null? aes-keys)
          current-scales
          (let* ((key (car aes-keys))
                 (aes-value (aes-get aesthetic-map key))
                 (rest-keys (cdr aes-keys)))
            ;; Only train if aesthetic is mapped to a column
            (if (and aes-value (symbol? aes-value))
                (let ((column-data (data-column data aes-value)))
                  ;; Determine scale type based on data
                  (let-values (((scale new-scales)
                                (get-or-create-scale 
                                 current-scales 
                                 key
                                 (lambda ()
                                   (if (number? (car column-data))
                                       (make-scale-linear)
                                       (make-scale-band))))))
                    (scale-train! scale column-data)
                    (loop rest-keys new-scales)))
                (loop rest-keys current-scales))))))

  ;;; ========================================================================
  ;;; geom-point: Scatter plots
  ;;; ========================================================================

(define (geom-point data aesthetic-map 
                    #!key (scales '()) (size 4) (shape 'circle) (alpha 1.0))
  "Create point geometry (scatter plot)
   Required aesthetics: x, y
   Optional aesthetics: color, fill, size"
  
  ;; Train scales
  (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
    
    ;; Get scales
    (let ((x-scale (cdr (assoc 'x trained-scales)))
          (y-scale (cdr (assoc 'y trained-scales)))
          (color-scale (let ((c (assoc 'color trained-scales)))
                         (if c (cdr c) #f))))
      
      ;; Generate points for each row
      (values
       (apply combine
              (map (lambda (row)
                     (let* ((x-val (aes-map-value aesthetic-map 'x row))
                            (y-val (aes-map-value aesthetic-map 'y row))
                            (color-val (aes-map-value aesthetic-map 'color row))
                            (size-val (or (aes-map-value aesthetic-map 'size row) size))
                            (x-pos (scale-map x-scale x-val))
                            (y-pos (scale-map y-scale y-val))
                            ;; Check if color aesthetic is mapped to data column
                            (color (cond
                                    ;; Color mapped to data + scale exists
                                    ((and color-val 
                                          color-scale 
                                          (symbol? (aes-get aesthetic-map 'color)))
                                     (scale-map color-scale color-val))
                                    ;; Static color value
                                    (color-val color-val)
                                    ;; Default
                                    (else "black"))))
                       (circle x-pos y-pos size-val
                              #:fill-color color
                              #:edge-color color)))
                   (data-rows data)))
       trained-scales))))

  ;;; ========================================================================
  ;;; geom-line: Line plots
  ;;; ========================================================================

(define (geom-line data aesthetic-map
                   #!key (scales '()) (width 2) (style 'solid))
  "Create line geometry
   Required aesthetics: x, y
   Optional aesthetics: color, linetype, group"
  
  ;; Train scales
  (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))

    ;; Get scales
    (let ((x-scale (cdr (assoc 'x trained-scales)))
          (y-scale (cdr (assoc 'y trained-scales)))
          (color-scale (let ((c (assoc 'color trained-scales)))
                        (if c (cdr c) #f))))
      
      ;; Check for grouping
      (let* ((explicit-group (aes-get aesthetic-map 'group))
             (group-aes-key (if explicit-group 'group 'color))
             (color-mapping (aes-get aesthetic-map 'color))
             (group-col (or explicit-group
                            (and (symbol? color-mapping) color-mapping))))
        
        (if group-col
            ;; Grouped lines
            (let* ((groups (delete-duplicates (data-column data group-col)))
                   ;; Draw separate colored line for each group
                   (group-lines
                    (map (lambda (g)
                           (let* ((group-rows 
                                   (filter (lambda (row)
                                             ;; Use group-col instead of literal 'group
                                             (equal? (aes-map-value aesthetic-map 
                                                                    group-aes-key row)
                                                     g))
                                           (data-rows data)))
                                  ;; Determine line color from scale
                                  (line-color
                                   (cond
                                    ;; If grouping by color and color scale exists
                                    ((and color-scale 
                                          (eq? group-col color-mapping))
                                     (scale-map color-scale g))
                                    ;; Default fallback
                                    (else "black"))))
                             (polyline
                              (map (lambda (row)
                                     (cons (scale-map x-scale 
                                                     (aes-map-value aesthetic-map 'x row))
                                           (scale-map y-scale 
                                                     (aes-map-value aesthetic-map 'y row))))
                                   group-rows)
                              #:color line-color  ; Apply group color
                              #:width width
                              #:style style)))
                         groups)))

              (values (apply combine group-lines) trained-scales))
            
            ;; Single line
            (values
             (polyline
              (map (lambda (row)
                     (cons (scale-map x-scale (aes-map-value aesthetic-map 'x row))
                           (scale-map y-scale (aes-map-value aesthetic-map 'y row))))
                   (data-rows data))
              #:width width
              #:style style)
             trained-scales))))))


  ;;; ========================================================================
  ;;; geom-path: Connected path
  ;;; ========================================================================

  (define (geom-path data aesthetic-map
                     #!key (scales '()) (width 2) (color "black"))
    "Create path geometry (like line but respects data order)
     Required aesthetics: x, y"
    (geom-line data aesthetic-map #:scales scales #:width width))

  ;;; ========================================================================
  ;;; geom-area: Area plot
  ;;; ========================================================================

  (define (geom-area data aesthetic-map
                   #!key (scales '()) (fill "gray") (alpha 0.5))
    "Create filled area geometry
   
   Two modes:
   1. Simple area (x, y): Fill from 0 to y
   2. Ribbon (x, ymin, ymax): Fill between ymin and ymax"
  
    (let ((has-ymin (aes-get aesthetic-map 'ymin))
          (has-ymax (aes-get aesthetic-map 'ymax)))
      
      (if (and has-ymin has-ymax)
          (geom-area-ribbon data aesthetic-map scales fill alpha)
          (geom-area-simple data aesthetic-map scales fill alpha))))


  (define (geom-area-simple data aesthetic-map scales fill alpha)
    "Simple area: fill from 0 to y values"
  
    ;; Train scales using standard mechanism
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
    
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        ;; Ensure y-domain includes 0 for area baseline
        (let* ((current-domain (scale-domain y-scale))
               (domain-min (car current-domain))
               (domain-max (cdr current-domain))
               (extended-domain (cons (min 0 domain-min)
                                      (max 0 domain-max))))
        
          (scale-set-domain! y-scale extended-domain)
          
          ;; Create polygon from data + baseline
          (let* ((rows (data-rows data))
                 (points (map (lambda (row)
                                (cons (scale-map x-scale (aes-map-value aesthetic-map 'x row))
                                      (scale-map y-scale (aes-map-value aesthetic-map 'y row))))
                              rows))
                 (baseline-y (scale-map y-scale 0))
                 (first-x (caar points))
                 (last-x (car (last points)))
                 ;; Close polygon with baseline
                 (polygon-points (append points
                                         (list (cons last-x baseline-y)
                                               (cons first-x baseline-y)))))
            (values
             (polygon polygon-points #:fill-color fill)
             trained-scales))))))



(define (geom-area-ribbon-lines data aesthetic-map scales fill alpha)
  "Test version: draw boundaries as lines instead of filled polygon"
  
  (let ((x-scale (cdr (assoc 'x scales)))
        (y-scale (cdr (assoc 'y scales))))
    
    (let ((x-col (aes-get aesthetic-map 'x))
          (ymin-col (aes-get aesthetic-map 'ymin))
          (ymax-col (aes-get aesthetic-map 'ymax)))
      
      (let ((xs (data-column data x-col))
            (ymins (data-column data ymin-col))
            (ymaxs (data-column data ymax-col)))
        
        (let ((x-pixels (map (lambda (x) (scale-map x-scale x)) xs))
              (ymin-pixels (map (lambda (y) (scale-map y-scale y)) ymins))
              (ymax-pixels (map (lambda (y) (scale-map y-scale y)) ymaxs)))
          
          ;; Draw upper boundary as a line
          (let ((upper-line (polyline (map cons x-pixels ymax-pixels)
                                     #:color "blue" #:width 2))
                ;; Draw lower boundary as a line
                (lower-line (polyline (map cons x-pixels ymin-pixels)
                                     #:color "red" #:width 2)))
            
            (values
             (combine upper-line lower-line)
             (list (cons 'x x-scale) (cons 'y y-scale)))))))))

  
  (define (geom-area-ribbon data aesthetic-map scales fill alpha)
    "Ribbon mode: fill between ymin and ymax curves"
  
    ;; Check if scales were provided
    (if (null? scales)
        ;; No scales provided - need to create and train them
        (let* ((x-col (aes-get aesthetic-map 'x))
               (ymin-col (aes-get aesthetic-map 'ymin))
               (ymax-col (aes-get aesthetic-map 'ymax))
               (x-data (data-column data x-col))
               (ymin-data (data-column data ymin-col))
               (ymax-data (data-column data ymax-col))
               (x-scale (make-scale-linear))
               (y-scale (make-scale-linear)))
          
          (scale-train! x-scale x-data)
          (scale-train! y-scale (append ymin-data ymax-data))
          
          (geom-area-ribbon-with-scales data aesthetic-map 
                                        x-scale y-scale fill alpha))
        
        ;; Scales provided - use them directly
        (let ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales))))
          (geom-area-ribbon-with-scales data aesthetic-map 
                                        x-scale y-scale fill alpha))))

  (define (geom-area-ribbon-with-scales data aesthetic-map x-scale y-scale fill alpha)
    "Helper: Create ribbon polygon using provided scales"
  
    (let ((trained-scales (list (cons 'x x-scale) (cons 'y y-scale))))
      
      ;; Sort rows by x value to ensure proper polygon winding
      (let* ((rows (data-rows data))
             (x-col (aes-get aesthetic-map 'x))
             (sorted-rows (sort rows
                                (lambda (row1 row2)
                                  (let ((x1 (aes-map-value aesthetic-map 'x row1))
                                        (x2 (aes-map-value aesthetic-map 'x row2)))
                                    (< x1 x2))))))

        ;; Create polygon between ymin and ymax curves
        (let* (;; Upper curve (ymax): left to right
               (upper-points (map (lambda (row)
                                    (cons (scale-map x-scale 
                                                     (aes-map-value aesthetic-map 'x row))
                                          (scale-map y-scale 
                                                     (aes-map-value aesthetic-map 'ymax row))))
                                  sorted-rows))
               ;; Lower curve (ymin): right to left (reversed)
               (lower-points (reverse
                              (map (lambda (row)
                                     (cons (scale-map x-scale 
                                                      (aes-map-value aesthetic-map 'x row))
                                           (scale-map y-scale 
                                                      (aes-map-value aesthetic-map 'ymin row))))
                                   sorted-rows)))
               ;; Combine to form closed polygon
               (polygon-points (append upper-points lower-points)))
          
          (values
           (polygon polygon-points #:fill-color fill)
           trained-scales)))))
  
  
  ;;; ========================================================================
  ;;; geom-bar: Bar chart
  ;;; ========================================================================

  (define (geom-bar data aesthetic-map
                    #!key (scales '()) (fill "steelblue") (width 0.8))
    "Create bar chart geometry
     Required aesthetics: x, y
     x should be categorical, y should be numeric"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
      ;; Ensure y-domain includes 0 for bars
        (let* ((current-domain (scale-domain y-scale))
               (domain-min (car current-domain))
               (domain-max (cdr current-domain))
               (extended-domain (cons (min 0 domain-min)
                                      (max 0 domain-max))))
          (scale-set-domain! y-scale extended-domain)
          
          ;; Generate bars
          (values
           (apply combine
                  (map (lambda (row)
                         (let* ((x-val (aes-map-value aesthetic-map 'x row))
                                (y-val (aes-map-value aesthetic-map 'y row))
                                (x-pos (scale-map x-scale x-val))
                                (y-pos (scale-map y-scale y-val))
                                (y-zero (scale-map y-scale 0))
                                (bar-width (* (scale-bandwidth x-scale) width)))
                           (rectangle (- x-pos (/ bar-width 2))
                                      y-zero
                                      bar-width
                                      (- y-pos y-zero)
                                      #:fill-color fill)))
                       (data-rows data)))
           trained-scales)))))

  ;;; ========================================================================
  ;;; geom-rect: Rectangles
  ;;; ========================================================================

  (define (geom-rect data aesthetic-map
                     #!key (scales '()) (fill "gray") (color "black"))
    "Create rectangle geometry
     Required aesthetics: xmin, xmax, ymin, ymax"
    
    ;; Train scales (need to handle xmin/xmax/ymin/ymax)
    (let* ((x-data (append (data-column data (aes-get aesthetic-map 'xmin))
                           (data-column data (aes-get aesthetic-map 'xmax))))
           (y-data (append (data-column data (aes-get aesthetic-map 'ymin))
                           (data-column data (aes-get aesthetic-map 'ymax))))
           (x-scale (make-scale-linear))
           (y-scale (make-scale-linear)))

      (scale-train! x-scale x-data)
      (scale-train! y-scale y-data)
      
      (let ((trained-scales (list (cons 'x x-scale) (cons 'y y-scale))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((xmin (scale-map x-scale 
                                              (aes-map-value aesthetic-map 'xmin row)))
                              (xmax (scale-map x-scale 
                                              (aes-map-value aesthetic-map 'xmax row)))
                              (ymin (scale-map y-scale 
                                              (aes-map-value aesthetic-map 'ymin row)))
                              (ymax (scale-map y-scale 
                                              (aes-map-value aesthetic-map 'ymax row))))
                         (rectangle xmin ymin 
                                   (- xmax xmin) 
                                   (- ymax ymin)
                                   #:fill-color fill
                                   #:edge-color color)))
                     (data-rows data)))
         trained-scales))))

  ;;; ========================================================================
  ;;; geom-text: Text labels
  ;;; ========================================================================

  (define (geom-text data aesthetic-map
                     #!key (scales '()) (size 10.0) (color "black"))
    "Create text geometry
     Required aesthetics: x, y, label"
    
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((x-val (aes-map-value aesthetic-map 'x row))
                              (y-val (aes-map-value aesthetic-map 'y row))
                              (label (aes-map-value aesthetic-map 'label row))
                              (x-pos (scale-map x-scale x-val))
                              (y-pos (scale-map y-scale y-val)))
                         (text x-pos y-pos (format "~a" label)
                               #:color color
                               #:size size)))
                     (data-rows data)))
         trained-scales))))

  ;;; ========================================================================
  ;;; geom-segment: Line segments
  ;;; ========================================================================

  (define (geom-segment data aesthetic-map
                        #!key (scales '()) (color "black") (width 1))
    "Create line segment geometry
     Required aesthetics: x, y, xend, yend"
    
    ;; Train scales
    (let* ((x-data (append (data-column data (aes-get aesthetic-map 'x))
                          (data-column data (aes-get aesthetic-map 'xend))))
           (y-data (append (data-column data (aes-get aesthetic-map 'y))
                          (data-column data (aes-get aesthetic-map 'yend))))
           (x-scale (make-scale-linear))
           (y-scale (make-scale-linear)))
      
      (scale-train! x-scale x-data)
      (scale-train! y-scale y-data)
      
      (let ((trained-scales (list (cons 'x x-scale) (cons 'y y-scale))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((x1 (scale-map x-scale 
                                            (aes-map-value aesthetic-map 'x row)))
                              (y1 (scale-map y-scale 
                                            (aes-map-value aesthetic-map 'y row)))
                              (x2 (scale-map x-scale 
                                            (aes-map-value aesthetic-map 'xend row)))
                              (y2 (scale-map y-scale 
                                            (aes-map-value aesthetic-map 'yend row))))
                         (line x1 y1 x2 y2 #:color color #:width width)))
                     (data-rows data)))
         trained-scales))))

  ;;; ========================================================================
  ;;; geom-hline / geom-vline: Reference lines
  ;;; ========================================================================

  (define (geom-hline yintercept
                      #!key (scales '()) (color "black") (width 1) (style 'solid))
    "Create horizontal reference line at y-intercept"
    (lambda (x-scale y-scale)
      (let ((y-pos (scale-map y-scale yintercept))
            (x-range (scale-range x-scale)))
        (line (car x-range) y-pos (cdr x-range) y-pos
              #:color color #:width width #:style style))))

  (define (geom-vline xintercept
                      #!key (scales '()) (color "black") (width 1) (style 'solid))
    "Create vertical reference line at x-intercept"
    (lambda (x-scale y-scale)
      (let ((x-pos (scale-map x-scale xintercept))
            (y-range (scale-range y-scale)))
        (line x-pos (car y-range) x-pos (cdr y-range)
              #:color color #:width width #:style style))))

  ;;; ========================================================================
  ;;; geom-eventplot: Neural spike raster
  ;;; ========================================================================

  (define (geom-eventplot data aesthetic-map
                          #!key (scales '()) (line-length 2.8) 
                          (color "black") (width 4))
    "Create event plot (spike raster)
     Required aesthetics: x (event times), y (trial/unit)
     Optional aesthetics: color
     x values should be lists of event times"
  
    ;; Train scales
    (let ((trained-scales (if (null? scales)
                              ;; No scales provided - need to train manually
                              (let* ((event-col (aes-get aesthetic-map 'x))
                                     (trial-col (aes-get aesthetic-map 'y))
                                     (all-events (apply append (data-column data event-col)))
                                     (x-scale (make-scale-linear))
                                     (y-scale (make-scale-linear)))
                                (scale-train! x-scale all-events)
                                (scale-train! y-scale (data-column data trial-col))
                                (list (cons 'x x-scale) (cons 'y y-scale)))
                              ;; Scales provided - use them directly
                              scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales)))
            (color-scale (let ((c (assoc 'color trained-scales)))
                           (if c (cdr c) #f))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((events (aes-map-value aesthetic-map 'x row))
                              (y-val (aes-map-value aesthetic-map 'y row))
                              (y-pos (scale-map y-scale y-val))
                              (half-h (/ line-length 2))
                              ;; Get color value from aesthetic or use default
                              (color-val (aes-map-value aesthetic-map 'color row))
                              ;; Map color through scale if available
                              (line-color
                               (cond
                                ;; Color mapped to data + scale exists
                                ((and color-val 
                                      color-scale 
                                      (symbol? (aes-get aesthetic-map 'color)))
                                 (scale-map color-scale color-val))
                                ;; Static color value from aesthetic
                                (color-val color-val)
                                ;; Default parameter
                                (else color))))
                         
                         (if (list? events)
                             (apply combine
                                    (map (lambda (event-time)
                                           (let ((x-pos (scale-map x-scale event-time)))
                                             (line x-pos (- y-pos half-h)
                                                   x-pos (+ y-pos half-h)
                                                   #:color line-color
                                                   #:width width)))
                                         events))
                             empty-drawer)))
                     (data-rows data)))
         trained-scales))))


  ;;; ========================================================================
  ;;; Geometry Combinators
  ;;; ========================================================================

  (define (geom-with-ranges geom-fn data aes-map x-range y-range #!rest args)
    "Wrapper that pre-configures scales with ranges before creating geometry"
    (let* ((x-col (aes-get aes-map 'x))
           (y-col (aes-get aes-map 'y))
           (x-scale (if (symbol? x-col)
                        (let ((col-data (data-column data x-col)))
                          (if (number? (car col-data))
                              (scale-with-range
                               (scale-with-trained (make-scale-linear) col-data)
                               x-range)
                              (scale-with-range
                               (scale-with-trained (make-scale-band) col-data)
                               x-range)))
                        #f))
           (y-scale (if (symbol? y-col)
                        (let ((col-data (data-column data y-col)))
                          (if (number? (car col-data))
                              (scale-with-range
                               (scale-with-trained (make-scale-linear) col-data)
                               y-range)
                              (scale-with-range
                               (scale-with-trained (make-scale-band) col-data)
                               y-range)))
                        #f))
           (scales (list (cons 'x x-scale) (cons 'y y-scale))))
      (apply geom-fn data aes-map #:scales scales args)))

;;; ========================================================================
;;; Statistical Geometries
;;; ========================================================================

;;; ------------------------------------------------------------------------
;;; geom-histogram: Render histogram from preprocessed bin data
;;; ------------------------------------------------------------------------

  (define (geom-histogram data aesthetic-map
                          #!key (scales '()) (fill "steelblue") 
                          (color "black") (alpha 0.9))
    "Render histogram from preprocessed bin data
   
     Required aesthetics: 
       x (bin centers), width (bin widths), y (counts)
   
     Data format (from stat-bin):
     ((x . (center1 center2...))
      (width . (w1 w2...))
      (y . (count1 count2...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        ;; Ensure y-domain includes 0 for histogram baseline
        (let* ((current-domain (scale-domain y-scale))
               (domain-min (car current-domain))
               (domain-max (cdr current-domain))
               (extended-domain (cons (min 0 domain-min)
                                      (max 0 domain-max))))
          
          (scale-set-domain! y-scale extended-domain)
          
          ;; Generate rectangles for each bin
          (values
           (apply combine
                  (map (lambda (row)
                         (let* ((x-center (aes-map-value aesthetic-map 'x row))
                                (bin-width (aes-map-value aesthetic-map 'width row))
                                (count (aes-map-value aesthetic-map 'y row))
                                (x-pos (scale-map x-scale x-center))
                                (y-pos (scale-map y-scale count))
                                (y-zero (scale-map y-scale 0))
                                (half-width (/ bin-width 2)))
                           
                           ;; Draw rectangle from baseline to count height
                           (rectangle (- x-pos (/ (scale-map x-scale (+ x-center half-width))
                                                  (scale-map x-scale (- x-center half-width))))
                                      y-zero
                                      (- (scale-map x-scale (+ x-center half-width))
                                         (scale-map x-scale (- x-center half-width)))
                                      (- y-pos y-zero)
                                      #:fill-color fill
                                      #:edge-color color)))
                       (data-rows data)))
           trained-scales)))))
  
;;; ------------------------------------------------------------------------
;;; geom-density: Render density curve from KDE data
;;; ------------------------------------------------------------------------

  (define (geom-density data aesthetic-map
                        #!key (scales '()) (fill "lightblue") 
                        (alpha 0.5) (color "navy") (geom-type 'area))
    "Render density curve from preprocessed KDE data
   
     Required aesthetics: x (evaluation points), y (density values)
   
     geom-type: 'area (filled) or 'line (curve only)
   
     Data format (from stat-density):
       ((x . (x1 x2 x3...))
       (y . (density1 density2 density3...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        ;; Ensure y-domain includes 0 for density baseline
        (let* ((current-domain (scale-domain y-scale))
               (domain-min (car current-domain))
               (domain-max (cdr current-domain))
               (extended-domain (cons (min 0 domain-min)
                                      (max 0 domain-max))))
          
          (scale-set-domain! y-scale extended-domain)
          
          (let* ((rows (data-rows data))
                 (points (map (lambda (row)
                                (cons (scale-map x-scale 
                                                 (aes-map-value aesthetic-map 'x row))
                                      (scale-map y-scale 
                                                 (aes-map-value aesthetic-map 'y row))))
                              rows)))
            
            (values
             (case geom-type
               ((area)
                ;; Filled area under density curve
                (let* ((baseline-y (scale-map y-scale 0))
                       (first-x (caar points))
                       (last-x (car (last points)))
                       ;; Close polygon with baseline
                       (polygon-points (append points
                                               (list (cons last-x baseline-y)
                                                     (cons first-x baseline-y)))))
                  (polygon polygon-points #:fill-color fill)))
               
               ((line)
                ;; Just the density curve
                (polyline points #:color color #:width 2))
               
               (else
                (error "geom-density: unknown geom-type" geom-type)))
             trained-scales))))))

;;; ------------------------------------------------------------------------
;;; geom-boxplot: Render box plot from summary statistics
;;; ------------------------------------------------------------------------

  (define (geom-boxplot data aesthetic-map
                        #!key (scales '()) (fill "white") 
                        (color "black") (outlier-color "red")
                        (outlier-size 3) (width 0.6))
    "Render box plot from preprocessed summary statistics
   
     Required aesthetics: 
       x (group), ymin (lower whisker), lower (Q1), 
       middle (median), upper (Q3), ymax (upper whisker)
   
     Optional aesthetics: 
       outliers (list of outlier values per group)
   
     Data format (from stat-boxplot):
       ((x . (group1 group2...))
        (ymin . (whisker1 whisker2...))
        (lower . (q1-1 q1-2...))
        (middle . (median1 median2...))
        (upper . (q3-1 q3-2...))
        (ymax . (whisker-upper1 whisker-upper2...))
        (outliers . ((outlier-list1) (outlier-list2)...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((x-val (aes-map-value aesthetic-map 'x row))
                              (ymin (aes-map-value aesthetic-map 'ymin row))
                              (lower (aes-map-value aesthetic-map 'lower row))
                              (middle (aes-map-value aesthetic-map 'middle row))
                              (upper (aes-map-value aesthetic-map 'upper row))
                              (ymax (aes-map-value aesthetic-map 'ymax row))
                              (outliers-list (aes-map-value aesthetic-map 'outliers row))
                              (x-pos (scale-map x-scale x-val))
                              (box-width (* (scale-bandwidth x-scale) width))
                              (half-width (/ box-width 2)))
                         
                         ;; Combine all box plot elements
                         (combine
                          ;; Lower whisker (vertical line from ymin to Q1)
                          (line x-pos (scale-map y-scale ymin)
                                x-pos (scale-map y-scale lower)
                                #:color color #:width 1)
                          
                          ;; Lower whisker cap (horizontal line at ymin)
                          (line (- x-pos half-width) (scale-map y-scale ymin)
                                (+ x-pos half-width) (scale-map y-scale ymin)
                                #:color color #:width 1)
                          
                          ;; Box (Q1 to Q3)
                          (rectangle (- x-pos half-width)
                                     (scale-map y-scale lower)
                                     box-width
                                     (- (scale-map y-scale upper)
                                        (scale-map y-scale lower))
                                     #:fill-color fill
                                     #:edge-color color)
                          
                          ;; Median line
                          (line (- x-pos half-width) (scale-map y-scale middle)
                                (+ x-pos half-width) (scale-map y-scale middle)
                                #:color color #:width 2)
                          
                          ;; Upper whisker (vertical line from Q3 to ymax)
                          (line x-pos (scale-map y-scale upper)
                                x-pos (scale-map y-scale ymax)
                                #:color color #:width 1)
                          
                          ;; Upper whisker cap
                          (line (- x-pos half-width) (scale-map y-scale ymax)
                                (+ x-pos half-width) (scale-map y-scale ymax)
                                #:color color #:width 1)
                          
                          ;; Outliers (if present)
                          (if (and outliers-list (list? outliers-list))
                              (apply combine
                                     (map (lambda (outlier-val)
                                            (circle x-pos 
                                                    (scale-map y-scale outlier-val)
                                                    outlier-size
                                                    #:fill-color outlier-color
                                                    #:edge-color outlier-color))
                                          outliers-list))
                              empty-drawer))))
                     (data-rows data)))
         trained-scales))))
  
;;; ------------------------------------------------------------------------
;;; geom-violin: Render violin plot (density + boxplot overlay)
;;; ------------------------------------------------------------------------

  (define (geom-violin data aesthetic-map
                       #!key (scales '()) (fill "lightgray")
                       (color "black") (scale-width #t) (draw-quantiles #f))
    "Render violin plot from preprocessed density data
   
     Required aesthetics: x (group), y (density grid values)
   
     This is a complex geometry that combines density estimation
     with optional boxplot overlay.
   
     Data format (from stat-violin):
      Nested structure with density curves per group
       ((group . group1)
       (density . ((x . (...)) (y . (...))))
       (boxplot . (...)))
     ..."
  
    ;; This is a placeholder - full implementation requires
    ;; handling the nested data structure from stat-violin
    ;; For now, delegate to simpler rendering
  
    (error 'geom-violin "Not yet fully implemented - use geom-boxplot"))

;;; ------------------------------------------------------------------------
;;; geom-errorbar: Render error bars
;;; ------------------------------------------------------------------------

  (define (geom-errorbar data aesthetic-map
                         #!key (scales '()) (color "black") 
                         (width 2) (cap-width 0.1))
    "Render error bars from preprocessed confidence intervals
   
     Required aesthetics: 
       x, y (point location), ymin, ymax (error bounds)
   
     Optional aesthetics: 
       xmin, xmax (for horizontal error bars)
   
     Data format (from stat-summary or pre-computed):
       ((x . (x1 x2...))
        (y . (y1 y2...))
        (ymin . (lower1 lower2...))
        (ymax . (upper1 upper2...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((x-val (aes-map-value aesthetic-map 'x row))
                              (y-val (aes-map-value aesthetic-map 'y row))
                              (ymin (aes-map-value aesthetic-map 'ymin row))
                              (ymax (aes-map-value aesthetic-map 'ymax row))
                              (x-pos (scale-map x-scale x-val))
                              (y-pos (scale-map y-scale y-val))
                              (y-min-pos (scale-map y-scale ymin))
                              (y-max-pos (scale-map y-scale ymax))
                              ;; Compute cap width - handle both categorical and continuous
                              (cap-half-width 
                               ;; Try to get bandwidth - only band scales support this
                               (handle-exceptions
                                exn
                                ;; Linear scale (continuous) - bandwidth not supported
                                ;; Use fraction of pixel range instead
                                (let ((x-range (scale-range x-scale)))
                                  (* cap-width 
                                     (- (cdr x-range) (car x-range))
                                     0.02))
                                ;; Try to get bandwidth from band scale
                                (let ((bandwidth (scale-bandwidth x-scale)))
                                  (if (and bandwidth (> bandwidth 0))
                                      (* cap-width bandwidth)
                                      ;; Fallback to pixel range
                                      (let ((x-range (scale-range x-scale)))
                                        (* cap-width 
                                           (- (cdr x-range) (car x-range))
                                           0.02)))))))
                         
                         (combine
                          ;; Vertical line from ymin to ymax
                          (line x-pos y-min-pos x-pos y-max-pos
                                #:color color #:width width)
                          
                          ;; Lower cap
                          (line (- x-pos cap-half-width)
                                y-min-pos
                                (+ x-pos cap-half-width)
                                y-min-pos
                                #:color color #:width width)
                          
                          ;; Upper cap
                          (line (- x-pos cap-half-width)
                                y-max-pos
                                (+ x-pos cap-half-width)
                                y-max-pos
                                #:color color #:width width))))
                     (data-rows data)))
         trained-scales))
      ))

  
;;; ------------------------------------------------------------------------
;;; geom-crossbar: Render crossbar (short boxplot bar)
;;; ------------------------------------------------------------------------

  (define (geom-crossbar data aesthetic-map
                         #!key (scales '()) (fill "white")
                         (color "black") (width 0.5))
    "Render crossbar from summary statistics
   
     Required aesthetics: x, y (center), ymin, ymax
   
     A crossbar is a horizontal bar with a center line,
     simpler than a full boxplot.
   
     Data format:
       ((x . (x1 x2...))
       (y . (center1 center2...))
       (ymin . (lower1 lower2...))
       (ymax . (upper1 upper2...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        (values
       (apply combine
              (map (lambda (row)
                     (let* ((x-val (aes-map-value aesthetic-map 'x row))
                            (y-val (aes-map-value aesthetic-map 'y row))
                            (ymin (aes-map-value aesthetic-map 'ymin row))
                            (ymax (aes-map-value aesthetic-map 'ymax row))
                            (x-pos (scale-map x-scale x-val))
                            (y-pos (scale-map y-scale y-val))
                            (y-min-pos (scale-map y-scale ymin))
                            (y-max-pos (scale-map y-scale ymax))
                            (bar-width (* (scale-bandwidth x-scale) width))
                            (half-width (/ bar-width 2)))
                       
                       (combine
                        ;; Vertical bar from ymin to ymax
                        (rectangle (- x-pos half-width)
                                   y-min-pos
                                   bar-width
                                   (- y-max-pos y-min-pos)
                                   #:fill-color fill
                                   #:edge-color color)
                        
                        ;; Center line at y
                        (line (- x-pos half-width) y-pos
                              (+ x-pos half-width) y-pos
                              #:color color #:width 2))))
                   (data-rows data)))
       trained-scales))))
  
;;; ------------------------------------------------------------------------
;;; geom-linerange: Render vertical/horizontal line ranges
;;; ------------------------------------------------------------------------
  
  (define (geom-linerange data aesthetic-map
                          #!key (scales '()) (color "black") (width 2))
    "Render vertical or horizontal line ranges
   
     Required aesthetics (vertical): x, ymin, ymax
     Required aesthetics (horizontal): y, xmin, xmax
   
     Data format (vertical):
       ((x . (x1 x2...))
        (ymin . (lower1 lower2...))
        (ymax . (upper1 upper2...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        ;; Check if vertical or horizontal
        (let ((has-ymin (aes-get aesthetic-map 'ymin))
              (has-xmin (aes-get aesthetic-map 'xmin)))
          
          (values
           (apply combine
                  (map (lambda (row)
                         (if has-ymin
                             ;; Vertical line range
                             (let* ((x-val (aes-map-value aesthetic-map 'x row))
                                    (ymin (aes-map-value aesthetic-map 'ymin row))
                                    (ymax (aes-map-value aesthetic-map 'ymax row))
                                    (x-pos (scale-map x-scale x-val)))
                               (line x-pos (scale-map y-scale ymin)
                                     x-pos (scale-map y-scale ymax)
                                     #:color color #:width width))
                             
                             ;; Horizontal line range
                             (let* ((y-val (aes-map-value aesthetic-map 'y row))
                                    (xmin (aes-map-value aesthetic-map 'xmin row))
                                    (xmax (aes-map-value aesthetic-map 'xmax row))
                                    (y-pos (scale-map y-scale y-val)))
                               (line (scale-map x-scale xmin) y-pos
                                     (scale-map x-scale xmax) y-pos
                                     #:color color #:width width))))
                       (data-rows data)))
           trained-scales)))))
  
;;; ------------------------------------------------------------------------
;;; geom-pointrange: Point with error bar (composite geometry)
;;; ------------------------------------------------------------------------
  
  (define (geom-pointrange data aesthetic-map
                           #!key (scales '()) (color "black")
                           (point-size 4) (line-width 2))
    "Render point with error bar
   
     Required aesthetics: x, y, ymin, ymax
   
     This is a composite geometry combining geom-point and geom-errorbar.
   
     Data format:
       ((x . (x1 x2...))
        (y . (y1 y2...))
        (ymin . (lower1 lower2...))
        (ymax . (upper1 upper2...)))"
  
    ;; Train scales
    (let ((trained-scales (train-scales-from-data data aesthetic-map scales)))
      
      (let ((x-scale (cdr (assoc 'x trained-scales)))
            (y-scale (cdr (assoc 'y trained-scales))))
        
        (values
         (apply combine
                (map (lambda (row)
                       (let* ((x-val (aes-map-value aesthetic-map 'x row))
                              (y-val (aes-map-value aesthetic-map 'y row))
                              (ymin (aes-map-value aesthetic-map 'ymin row))
                              (ymax (aes-map-value aesthetic-map 'ymax row))
                              (x-pos (scale-map x-scale x-val))
                              (y-pos (scale-map y-scale y-val)))
                         
                         (combine
                          ;; Error bar (line range)
                          (line x-pos (scale-map y-scale ymin)
                                x-pos (scale-map y-scale ymax)
                                #:color color #:width line-width)
                          
                          ;; Point at center
                          (circle x-pos y-pos point-size
                                  #:fill-color color
                                  #:edge-color color))))
                     (data-rows data)))
         trained-scales))))
  
) ; end module
