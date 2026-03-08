
;;;; ========================================================================
;;;; Helper Functions for Data Transformation
;;;; ========================================================================

(define (transpose-group-stats group-stats)
  "Convert list of group statistics to columnar format
   
   Input: List of alists, one per group
     '(((x . group1) (y . val1) (ymin . min1) ...)
       ((x . group2) (y . val2) (ymin . min2) ...)
       ...)
   
   Output: Columnar format
     ((x . (group1 group2 ...))
      (y . (val1 val2 ...))
      (ymin . (min1 min2 ...))
      ...)"
  
  (if (null? group-stats)
      '()
      (let ((keys (map car (car group-stats))))
        (map (lambda (key)
               (cons key (map (lambda (row) (cdr (assoc key row)))
                              group-stats)))
             keys))))

;;;; ========================================================================
;;;; Statistical Transformation Functions
;;;; ========================================================================

;;; ------------------------------------------------------------------------
;;; stat-bin: Transform raw data into histogram bins
;;; ------------------------------------------------------------------------

(define (stat-bin layer-data aes params)
  "Transform raw data into histogram bins
   
   Input aesthetics: x (raw values)
   Output columns: x (bin centers), width (bin widths), y (counts)
   
   Parameters:
     bins:     Number of bins (default 30)
     binwidth: Width of bins (overrides bins)
     breaks:   Explicit breakpoint vector
     method:   Binning method ('sturges, 'scott, 'fd, 'equal-width)"
 
  (let* ((x-col (aes-get aes 'x))
         (values (data-column layer-data x-col))
         (bins (or (assq-ref params 'bins) 30))
         (binwidth (assq-ref params 'binwidth))
         (breaks-param (assq-ref params 'breaks))
         (method (or (assq-ref params 'method) 'sturges)))
    
    ;; Determine breaks
    (let ((breaks
           (cond
            ;; Explicit breaks provided
            (breaks-param breaks-param)
            
            ;; Binwidth specified - compute breaks
            (binwidth
             (let* ((min-val (reduce* min values))
                    (max-val (reduce* max values))
                    (n-bins (inexact->exact 
                             (ceiling (/ (- max-val min-val) binwidth)))))
               (list->vector
                (map (lambda (i) (+ min-val (* i binwidth)))
                     (iota (+ n-bins 1))))))
            
            ;; Use histogram-breaks with specified method
            (else
             (histogram-breaks values bins #:method method)))))
      
      ;; Compute histogram
      (let-values (((bin-edges counts) (histogram values breaks)))
        
        ;; Compute bin centers and widths
        (let* ((n-bins (vector-length counts))
               (bin-centers (map (lambda (i)
                                   (/ (+ (vector-ref bin-edges i)
                                         (vector-ref bin-edges (+ i 1)))
                                      2.0))
                                 (iota n-bins)))
               (bin-widths (map (lambda (i)
                                  (- (vector-ref bin-edges (+ i 1))
                                     (vector-ref bin-edges i)))
                                (iota n-bins))))
          
          ;; Return preprocessed data in columnar format
          `((x . ,bin-centers)
            (width . ,bin-widths)
            (y . ,(vector->list counts))))))))

;;; ------------------------------------------------------------------------
;;; stat-density: Kernel density estimation
;;; ------------------------------------------------------------------------

(define (stat-density layer-data aes params)
  "Transform raw data into kernel density estimate
   
   Input aesthetics: x (raw values)
   Output columns: x (evaluation points), y (density values)
   
   Parameters:
     bandwidth: KDE bandwidth (auto-selected if #f)
     adjust:    Bandwidth adjustment factor (default 1.0)
     kernel:    Kernel type (default 'gaussian)
     n:         Number of evaluation points (default 512)"
  
  (let* ((x-col (aes-get aes 'x))
         (values (data-column layer-data x-col))
         (adjust (or (assq-ref params 'adjust) 1.0))
         (kernel (or (assq-ref params 'kernel) 'gaussian))
         (n (or (assq-ref params 'n) 512))
         (bandwidth-param (assq-ref params 'bandwidth)))
    
    ;; Compute bandwidth if not provided
    (let ((bandwidth (or bandwidth-param
                         (* adjust (kde-bandwidth-nrd values)))))
      
      ;; Compute KDE
      (let ((kde-result (kde values bandwidth kernel n)))
        
        ;; Convert list of pairs to columnar format
        `((x . ,(map car kde-result))
          (y . ,(map cdr kde-result)))))))

;;; ------------------------------------------------------------------------
;;; stat-boxplot: Compute boxplot summary statistics
;;; ------------------------------------------------------------------------

(define (stat-boxplot layer-data aes params)
  "Transform raw data into boxplot summary statistics
   
   Input aesthetics: x (group), y (values)
   Output columns: x, ymin, lower, middle, upper, ymax, outliers
   
   Parameters:
     coef: IQR multiplier for whisker length (default 1.5)"
  
  (let* ((x-col (aes-get aes 'x))
         (y-col (aes-get aes 'y))
         (groups (delete-duplicates (data-column layer-data x-col)))
         (coef (or (assq-ref params 'coef) 1.5)))
    
    ;; Compute statistics for each group
    (let ((group-stats
           (map (lambda (group)
                  (let* ((group-rows (filter (lambda (row)
                                               (equal? (cdr (assoc x-col row))
                                                      group))
                                             (data-rows layer-data)))
                         (y-values (map (lambda (row) (cdr (assoc y-col row)))
                                        group-rows)))
                    
                    ;; Check if we have enough data
                    (if (< (length y-values) 5)
                        ;; Not enough data for boxplot
                        `((x . ,group)
                          (ymin . ,(apply min y-values))
                          (lower . ,(apply min y-values))
                          (middle . ,(mean y-values))
                          (upper . ,(apply max y-values))
                          (ymax . ,(apply max y-values))
                          (outliers . ()))
                        
                        ;; Normal boxplot computation
                        (let* ((five-num (fivenum y-values)))
                          
                          (let-values (((outlier-vals clean-vals)
                                        (outliers y-values #:coef coef)))
                            
                            ;; Compute whiskers from clean values
                            (let* ((q1 (list-ref five-num 1))
                                   (q3 (list-ref five-num 3))
                                   (iqr-val (- q3 q1))
                                   (lower-fence (- q1 (* coef iqr-val)))
                                   (upper-fence (+ q3 (* coef iqr-val))))
                              
                              ;; Whiskers extend to most extreme data point
                              ;; within coef*IQR from quartiles
                              (let ((lower-whisker 
                                     (if (null? clean-vals)
                                         q1
                                         (apply min (filter (lambda (v) 
                                                              (>= v lower-fence))
                                                            clean-vals))))
                                    (upper-whisker 
                                     (if (null? clean-vals)
                                         q3
                                         (apply max (filter (lambda (v)
                                                              (<= v upper-fence))
                                                            clean-vals)))))
                                
                                `((x . ,group)
                                  (ymin . ,lower-whisker)
                                  (lower . ,q1)
                                  (middle . ,(list-ref five-num 2))
                                  (upper . ,q3)
                                  (ymax . ,upper-whisker)
                                  (outliers . ,outlier-vals)))))))))
                groups)))
      
      ;; Transpose to columnar format
      (transpose-group-stats group-stats)))
    )

;;; ------------------------------------------------------------------------
;;; stat-violin: Compute violin plot density grid
;;; ------------------------------------------------------------------------

(define (stat-violin layer-data aes params)
  "Transform raw data into violin plot density grid
   
   Input aesthetics: x (group), y (values)
   Output: Nested structure with density curves per group + boxplot stats
   
   Parameters:
     bandwidth: KDE bandwidth (auto-selected if #f)
     kernel:    Kernel type (default 'gaussian)
     scale:     Scaling method ('area or 'count)
     n:         Number of density evaluation points"
  
  (let* ((x-col (aes-get aes 'x))
         (y-col (aes-get aes 'y))
         (groups (delete-duplicates (data-column layer-data x-col)))
         (scale-method (or (assq-ref params 'scale) 'area))
         (kernel (or (assq-ref params 'kernel) 'gaussian))
         (n (or (assq-ref params 'n) 128))
         (bandwidth-param (assq-ref params 'bandwidth)))
    
    ;; Compute density + boxplot for each group
    (let ((group-results
           (map (lambda (group)
                  (let* ((group-rows (filter (lambda (row)
                                               (equal? (cdr (assoc x-col row))
                                                      group))
                                             (data-rows layer-data)))
                         (y-values (map (lambda (row) (cdr (assoc y-col row)))
                                        group-rows)))
                    
                    ;; Compute bandwidth for this group
                    (let ((bandwidth (or bandwidth-param
                                         (kde-bandwidth-nrd y-values))))
                      
                      ;; Compute density
                      (let ((density-pairs (kde y-values bandwidth kernel n)))
                        
                        `((group . ,group)
                          (density-x . ,(map car density-pairs))
                          (density-y . ,(map cdr density-pairs))
                          (y-values . ,y-values))))))
                groups)))
      
      ;; For now, return simplified format
      ;; Full violin implementation would mirror density on both sides
      ;; and overlay boxplot - this is complex and deferred
      
      ;; Return columnar format for basic rendering
      (transpose-group-stats group-results))))

;;; ------------------------------------------------------------------------
;;; stat-summary: Compute summary statistics with error bounds
;;; ------------------------------------------------------------------------

(define (stat-summary layer-data aes params)
  "Compute summary statistics with error bounds
   
   Input aesthetics: x (group), y (values)
   Output columns: x, y (summary), ymin, ymax
   
   Parameters:
     fun:      Summary function (default: mean)
     fun-ymin: Lower bound function (default: mean - SE)
     fun-ymax: Upper bound function (default: mean + SE)"
  
  (let* ((x-col (aes-get aes 'x))
         (y-col (aes-get aes 'y))
         (groups (delete-duplicates (data-column layer-data x-col)))
         (fun-y (or (assq-ref params 'fun-y) 
                    (assq-ref params 'fun)
                    mean))
         (fun-ymin (assq-ref params 'fun-ymin))
         (fun-ymax (assq-ref params 'fun-ymax)))
    
    ;; Default error functions: ±1 SE
    (let ((ymin-fn (or fun-ymin
                       (lambda (vals) 
                         (- (mean vals) 
                            (standard-error-of-the-mean vals)))))
          (ymax-fn (or fun-ymax
                       (lambda (vals)
                         (+ (mean vals)
                            (standard-error-of-the-mean vals))))))
      
      (let ((summaries
             (map (lambda (group)
                    (let* ((group-rows (filter (lambda (row)
                                                 (equal? (cdr (assoc x-col row))
                                                        group))
                                               (data-rows layer-data)))
                           (y-values (map (lambda (row) (cdr (assoc y-col row)))
                                          group-rows)))
                      
                      `((x . ,group)
                        (y . ,(fun-y y-values))
                        (ymin . ,(ymin-fn y-values))
                        (ymax . ,(ymax-fn y-values)))))
                  groups)))
        
        (transpose-group-stats summaries)))))


;;;; ========================================================================
;;;; Helper: Rendering Functions for Statistical Geometries
;;;; ========================================================================

;;; These helper functions are called from render-layer to render
;;; the statistical geometries with preprocessed data.

(define (render-geom-errorbar processed-data processed-aes scales params plotter)
  "Render errorbar geometry with preprocessed data"
  (let ((color (or (assq-ref params 'color) "black"))
        (width (or (assq-ref params 'width) 2))
        (cap-width (or (assq-ref params 'cap-width) 0.1)))
    (let-values (((drawer _) (geom-errorbar processed-data processed-aes
                                            #:scales scales
                                            #:color color
                                            #:width width
                                            #:cap-width cap-width)))
      (render-drawer drawer plotter))))

(define (render-geom-pointrange processed-data processed-aes scales params plotter)
  "Render pointrange geometry (point + error bar) with preprocessed data"
  (let ((color (or (assq-ref params 'color) "black"))
        (point-size (or (assq-ref params 'point-size) 4))
        (line-width (or (assq-ref params 'line-width) 2)))
    (let-values (((drawer _) (geom-pointrange processed-data processed-aes
                                              #:scales scales
                                              #:color color
                                              #:point-size point-size
                                              #:line-width line-width)))
      (render-drawer drawer plotter))))

(define (render-geom-linerange processed-data processed-aes scales params plotter)
  "Render linerange geometry with preprocessed data"
  (let ((color (or (assq-ref params 'color) "black"))
        (width (or (assq-ref params 'width) 2)))
    (let-values (((drawer _) (geom-linerange processed-data processed-aes
                                             #:scales scales
                                             #:color color
                                             #:width width)))
      (render-drawer drawer plotter))))

(define (render-geom-crossbar processed-data processed-aes scales params plotter)
  "Render crossbar geometry (horizontal bar with center line) with preprocessed data"
  (let ((fill (or (assq-ref params 'fill) "white"))
        (color (or (assq-ref params 'color) "black"))
        (width (or (assq-ref params 'width) 0.5)))
    (let-values (((drawer _) (geom-crossbar processed-data processed-aes
                                            #:scales scales
                                            #:fill fill
                                            #:color color
                                            #:width width)))
      (render-drawer drawer plotter))))


(define (render-geom-histogram processed-data processed-aes scales params plotter)
  "Render histogram geometry with preprocessed bin data"
  (let ((fill (or (assq-ref params 'fill) "steelblue"))
        (color (or (assq-ref params 'color) "black"))
        (alpha (or (assq-ref params 'alpha) 0.9)))
    (let-values (((drawer _) (geom-histogram processed-data processed-aes
                                             #:scales scales
                                             #:fill fill
                                             #:color color
                                             #:alpha alpha)))
      (render-drawer drawer plotter))))

(define (render-geom-density processed-data processed-aes scales params plotter)
  "Render density geometry with preprocessed KDE data"
  (let ((fill (or (assq-ref params 'fill) "lightblue"))
        (color (or (assq-ref params 'color) "navy"))
        (alpha (or (assq-ref params 'alpha) 0.5))
        (geom-type (or (assq-ref params 'geom-type) 'area)))
    (let-values (((drawer _) (geom-density processed-data processed-aes
                                           #:scales scales
                                           #:fill fill
                                           #:color color
                                           #:alpha alpha
                                           #:geom-type geom-type)))
      (render-drawer drawer plotter))))

(define (render-geom-boxplot processed-data processed-aes scales params plotter)
  "Render boxplot geometry with preprocessed statistics"
  (let ((fill (or (assq-ref params 'fill) "white"))
        (color (or (assq-ref params 'color) "black"))
        (outlier-color (or (assq-ref params 'outlier-color) "red"))
        (outlier-size (or (assq-ref params 'outlier-size) 3))
        (width (or (assq-ref params 'width) 0.6)))
    (let-values (((drawer _) (geom-boxplot processed-data processed-aes
                                           #:scales scales
                                           #:fill fill
                                           #:color color
                                           #:outlier-color outlier-color
                                           #:outlier-size outlier-size
                                           #:width width)))
      (render-drawer drawer plotter))))


(define (render-geom-violin processed-data processed-aes scales params plotter)
  "Render violin geometry (placeholder - delegates to boxplot for now)"
  ;; Full violin implementation is complex - for now use boxplot
  (render-geom-boxplot processed-data processed-aes scales params plotter))
