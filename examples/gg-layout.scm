;;;; Complete examples demonstrating gg-plot layout features
;;;;
;;;; These examples showcase label collision avoidance, legend positioning,
;;;; and annotation layers in realistic scientific visualization scenarios

(import scheme
        (chicken base)
        (chicken format)
        (chicken sort)
        (chicken random)
        yasos
        plot
        srfi-1
        srfi-13
        matchable
        datatype
        gg-primitives
        gg-scales
        gg-aes
        gg-geom
        gg-guides
        gg-plot
        gg-layout
        gg-plot)



  (define (assq-ref alist key)
    "Safe assq reference"
    (and (list? alist)
         (if (symbol? (car alist))
             (assq-ref (cdr alist) key)
             (let ((pair (assq key alist)))
               (if pair (cdr pair) #f)))))

;;; ========================================================================
;;; Helper Functions for Examples
;;; ========================================================================

(define (filter-by data pred col1 col2)
  "Filter data rows by predicate on two columns"
  (let* ((vals1 (assq-ref data col1))
         (vals2 (assq-ref data col2))
         (indices
          (filter (lambda (i)
                    (pred (list-ref vals1 i)
                          (list-ref vals2 i)))
                  (iota (length vals1)))))
    (map (lambda (col-pair)
           (cons (car col-pair)
                 (map (lambda (i) (list-ref (cdr col-pair) i))
                      indices)))
         data)))

(define (top-n-by data column n)
  "Select top n rows sorted by column"
  (let* ((values (assq-ref data column))
         (sorted-indices
          (sort (iota (length values))
                (lambda (i j)
                  (< (list-ref values i)
                     (list-ref values j))))))
    (map (lambda (col-pair)
           (cons (car col-pair)
                 (map (lambda (i) (list-ref (cdr col-pair) i))
                      (take sorted-indices n))))
         data)))

;;; ========================================================================
;;; Neuroscience Spike Raster with Event Annotations
;;; ========================================================================

(define spike-raster-example
  (let* ((;; Simulate spike train data
          spike-data
          '((trial . (0 1 2 3 4 5 6 7 8 9))
            (spikes . ((12 45 78 89)
                       (15 42 51 77 88 95)
                       (10 38 55 72)
                       (18 44 66 91)
                       (11 39 58 74 92)
                       (14 41 53 79 90)
                       (16 43 67 85)
                       (13 40 57 73 89 94)
                       (17 46 69 87)
                       (19 47 71 93)))
            ))
            ;(condition . ("baseline" "stim" "baseline" "stim" "baseline"))))
         
         (plot
          (ggplot spike-data (aes #:x 'spikes #:y 'trial)
            ;; Spike rasters as short vertical lines
            (layer-eventplot #:color "black" #:line-length 2.8 #:width 2.8)
            
            ;; Highlight stimulus period (20-70ms)
            (layer-annotate-rect 20 -0.5 70 4.5
                                 #:fill "yellow" #:alpha 0.15)
            
            ;; Mark stimulus onset (solid) and offset (dashed)
            (annotate-vline 20 #:color "red" #:linetype 'solid #:line-width 2)
            (annotate-vline 70 #:color "red" #:linetype 'dashed #:line-width 1.5)
            
            ;; Text labels for events
            (layer-annotate-text "Stim ON" 20 5.2
                                 #:color "red" #:size 11 #:hjust 0)
            (layer-annotate-text "Stim OFF" 70 5.2
                                 #:color "red" #:size 11 #:hjust 1)
            
            ;; Arrow showing response latency
            (layer-annotate-arrow 20 2.5 45 2.5
                                  #:color "darkgreen"
                                  #:arrow-length 12
                                  #:line-width 1.5)
            (layer-annotate-text "Response latency ~25ms" 32.5 3.3
                                 #:color "darkgreen" #:size 9)
            
            ;; Reference line at trial 0
            (annotate-hline 0 #:color "gray" #:linetype 'dotted)
            
            (scale-x-continuous #:label "Time (ms)" #:limits '(0 100))
            (scale-y-continuous #:label "Trial #" #:breaks '(0 1 2 3 4))
            (ggtitle "Peri-Stimulus Time Histogram - Single Neuron")
            (theme-minimal))))
    
    plot))


;;; ========================================================================
;;; Volcano Plot with Non-Overlapping Gene Labels
;;; ========================================================================

(define volcano-plot-example
  (let* ((;; Simulate differential expression data
          gene-data
          '((gene . ("ACTB" "GAPDH" "TP53" "MYC" "BRCA1" "EGFR" "KRAS"
                     "PTEN" "AKT1" "MAPK1" "CDK1" "CCNA2" "CDKN1A"
                     "BCL2" "BAX" "CASP3" "TNF" "IL6" "IFNG" "TLR4"))
            (log2fc . (-0.3 0.2 2.8 -2.1 3.5 -1.8 2.2
                       1.5 -2.5 1.8 2.9 3.1 -1.2
                       -2.8 2.3 1.9 -3.2 2.7 -1.5 1.7))
            (pvalue . (0.45 0.38 0.0008 0.015 0.0001 0.025 0.005
                       0.012 0.003 0.018 0.002 0.0005 0.055
                       0.001 0.008 0.020 0.0003 0.004 0.030 0.022))))
         
         ;; Compute -log10(p-value) for y-axis
         (neg-log10-pvalue
          (map (lambda (p) (- (log (max p 1e-10)) (log 10)))
               (assq-ref gene-data 'pvalue)))
         
         (enriched-data
          (append gene-data `((neg_log10_pvalue . ,neg-log10-pvalue))))
         
         ;; Filter significant genes (p < 0.01, |log2fc| > 1.5)
         (significant-genes
          (filter (lambda (i)
                    (let ((fc (list-ref (assq-ref enriched-data 'log2fc) i))
                          (pval (list-ref (assq-ref enriched-data 'pvalue) i)))
                      (and (< pval 0.01)
                           (> (abs fc) 1.5))))
                  (iota (length (assq-ref enriched-data 'gene)))))
         
         (significant-data
          `((gene . ,(map (lambda (i) (list-ref (assq-ref enriched-data 'gene) i))
                          significant-genes))
            (log2fc . ,(map (lambda (i) (list-ref (assq-ref enriched-data 'log2fc) i))
                            significant-genes))
            (neg_log10_pvalue . ,(map (lambda (i) (list-ref neg-log10-pvalue i))
                                      significant-genes))))
         
         (plot
          (ggplot enriched-data (aes #:x 'log2fc #:y 'neg_log10_pvalue)
            ;; All genes (gray)
            (layer-point #:color "gray" #:alpha 0.6 #:size 3)
            
            ;; Significant upregulated (red)
            (layer-point #:data (filter-by enriched-data
                                           (lambda (fc p) (and (< p 0.01) (> fc 1.5)))
                                           'log2fc 'pvalue)
                         #:color "red" #:size 5)
            
            ;; Significant downregulated (blue)
            (layer-point #:data (filter-by enriched-data
                                           (lambda (fc p) (and (< p 0.01) (< fc -1.5)))
                                           'log2fc 'pvalue)
                         #:color "blue" #:size 5)
            
            ;; Non-overlapping labels for significant genes
            (geom-text-repel significant-data
                             (aes #:x 'log2fc #:y 'neg_log10_pvalue #:label 'gene)
                             #:size 7
                             #:max-iterations 150)
            
            ;; Significance threshold (p = 0.01)
            (annotate-hline 2.0 #:color "red" #:linetype 'dashed)
            (layer-annotate-text "p = 0.01" -3.5 2.1
                                 #:color "red" #:size 9 #:hjust 0)
            
            ;; Fold-change cutoffs
            (annotate-vline -1.5 #:color "blue" #:linetype 'dotted)
            (annotate-vline 1.5 #:color "blue" #:linetype 'dotted)
            
            (scale-x-continuous #:label "log2(Fold Change)"
                                #:limits '(-4 4)
                                #:breaks '(-3 -1.5 0 1.5 3))
            (scale-y-continuous #:label "-log10(p-value)")
            (ggtitle "Differential Gene Expression: Treatment vs Control")
            (theme-minimal)
            (theme-legend-position 'none))))
    
    plot))


;;; ========================================================================
;;; Calibration Plot with Confidence Bands
;;; ========================================================================

(define calibration-plot-example
  (let* ((;; Simulate model predictions and observations
          n-points 50)
         (random-values
          (map (lambda (i)
                 (+ 5 (* 4 (sin (/ (* i 3.14159) 25)))
                    (* 0.8 (- (pseudo-random-real) 0.5))))
               (iota n-points)))
         
         (calibration-data
          `((predicted . ,(map (lambda (x) (+ x (* 0.3 (- (pseudo-random-real) 0.5))))
                               random-values))
            (observed . ,random-values)))
         
         ;; Compute regression line (simplified - use actual regression in practice)
         (slope 0.92)
         (intercept 0.5)
         
         ;; Confidence band bounds (simplified)
         (ci-lower '(0 1 2 3 4 5 6 7 8 9))
         (ci-upper '(2 3 4 5 6 7 8 9 10 11))
         
         (plot
          (ggplot calibration-data (aes #:x 'predicted #:y 'observed)
            ;; Scatter points
            (layer-point #:alpha 0.6 #:size 3.5 #:color "steelblue")
            
            ;; Confidence band (shaded region)
            (layer-annotate-rect 0 0.5 10 10.5
                                 #:fill "blue" #:alpha 0.08)
            
            ;; Perfect calibration line (identity)
            (annotate-abline 1.0 0.0
                             #:color "red"
                             #:linetype 'dashed
                             #:line-width 1.5)
            (layer-annotate-text "Perfect calibration" 9 9.3
                                 #:color "red" #:size 9 #:hjust 1)
            
            ;; Fitted regression line
            (annotate-abline slope intercept
                             #:color "blue"
                             #:line-width 2.5)
            
            ;; Model statistics annotation
            (layer-annotate-text "R^2 = 0.847 RMSE = 0.52" 7.5 1.5
                                 #:hjust 1 #:vjust 0
                                 #:size 10 #:color "black")
            
            ;; Arrow showing bias
            (layer-annotate-arrow 5 5 5.5 5.46
                                  #:color "darkgreen"
                                  #:arrow-length 10)
            (layer-annotate-text "Slight over-prediction" 5.7 5.5
                                 #:color "darkgreen" #:size 8)
            
            (scale-x-continuous #:label "Predicted Value" #:limits '(0 10))
            (scale-y-continuous #:label "Observed Value" #:limits '(0 10))
            (coord-fixed #:ratio 1)
            (ggtitle "Model Calibration Analysis")
            (theme-classic))))
    
    plot))

;;; ========================================================================
;;; EEG Time-Frequency with Inside Legend
;;; ========================================================================

(define eeg-timefreq-example
  (let* ((;; Simulate EEG time-frequency data
          times (map (lambda (i) (/ i 10.0)) (iota 100)))
         
         (eeg-data
          `((time . ,times)
            (theta . ,(map (lambda (t) (* 2 (cos (* t 2))))
                           times))
            (alpha . ,(map (lambda (t) (* 1.5 (sin (* t 1.5))))
                           times))
            (beta . ,(map (lambda (t) (* 1.2 (cos (* t 3))))
                          times))
            (gamma . ,(map (lambda (t) (* 0.8 (sin (* t 5))))
                           times))))
         
         ;; Convert to long format for plotting
         (long-data
          `((time . ,(append times times times times))
            (power . ,(append (assq-ref eeg-data 'theta)
                              (assq-ref eeg-data 'alpha)
                              (assq-ref eeg-data 'beta)
                              (assq-ref eeg-data 'gamma)))
            (band . ,(append (make-list 100 "theta")
                             (make-list 100 "alpha")
                             (make-list 100 "beta")
                             (make-list 100 "gamma")))))
         
         (plot
          (ggplot long-data (aes #:x 'time #:y 'power #:color 'band)
            ;; Power traces for each frequency band
            (layer-line #:width 1.2)
            
            ;; Baseline reference
            (annotate-hline 0 #:color "gray" #:linetype 'dotted #:line-width 0.8)
            
            ;; Event marker (stimulus at t=5)
            (annotate-vline 5.0 #:color "black" #:linetype 'solid #:line-width 2)
            (layer-annotate-text "Stimulus" 5.0 3.2
                                 #:size 10 #:hjust 0.5)
            
            ;; Highlight response period
            (layer-annotate-rect 5.0 -3 7.0 3
                                 #:fill "yellow" #:alpha 0.1)
            
            ;; Annotation for specific feature
            (layer-annotate-arrow 6.5 2.0 6.0 1.5
                                  #:color "purple"
                                  #:arrow-length 10)
            (layer-annotate-text "Alpha suppression" 6.6 2.2
                                 #:color "purple" #:size 8)
            
            (scale-color-manual #:values '(("theta" . "purple")
                                           ("alpha" . "blue")
                                           ("beta" . "green")
                                           ("gamma" . "red"))
                                #:name "Frequency Band")
            (scale-x-continuous #:label "Time (s)" #:limits '(0 10))
            (scale-y-continuous #:label "Normalized Power")
            (ggtitle "EEG Time-Frequency Analysis - Electrode Cz")
            
            ;; Legend inside to save space
            (theme-legend-inside 'top-right)
            (theme-minimal))))
    
    plot))

;;; ========================================================================
;;; Multi-Panel Box Plot with Multiple Annotations
;;; ========================================================================

(define boxplot-comparison-example
  (let* ((;; Simulate drug dose-response data
          doses '("10" "5" "1" "0"))
         (treatments '("placebo" "drug-A" "drug-B"))
         
          ;; Generate data for each dose × treatment combination
         (dose-response-data
          (let ((data '()))
            (for-each
             (lambda (dose)
               (for-each
                (lambda (treatment)
                  (let* ((n 20)
                         (base-response
                          (cond ((string=? treatment "placebo") 1.0)
                                ((string=? treatment "drug-A")
                                 (+ 1.0 (* 0.3 (string->number dose))))
                                ((string=? treatment "drug-B")
                                 (+ 1.0 (* 0.5 (string->number dose)))))))
                    (for-each
                     (lambda (i)
                       (set! data
                             (cons `((dose . ,dose)
                                     (treatment . ,treatment)
                                     (response . ,(+ base-response
                                                     (* 0.2 (- (pseudo-random-real) 0.5)))))
                                   data)))
                     (iota n))))
                treatments))
             doses)
            ;; Convert to columnar format
            `((dose . ,(map (lambda (row) (assq-ref row 'dose)) data))
              (treatment . ,(map (lambda (row) (assq-ref row 'treatment)) data))
              (response . ,(map (lambda (row) (assq-ref row 'response)) data)))))
         
         (plot
          (ggplot dose-response-data (aes #:x 'dose #:y 'response #:fill 'treatment)
            ;; Box plots with transparency
            (layer-boxplot #:alpha 0.7)
            
            ;; Baseline reference line
            (annotate-hline 1.0
                            #:color "black"
                            #:linetype 'solid
                            #:line-width 1.5)
            (layer-annotate-text "Baseline" -0.7 1.15
                                 #:size 11 #:hjust 0)
            
            ;; Therapeutic window
            (layer-annotate-rect -0.5 1.3 3.5 1.8
                                 #:fill "lightgreen"
                                 #:alpha 0.15
                                 #:color "darkgreen"
                                 #:line-width 1)
            (layer-annotate-text "Therapeutic window" 3.2 1.55
                                 #:color "darkgreen"
                                 #:size 11
                                 #:hjust 1)   ; right-align so text extends leftward into panel

            ;; Arrow showing dose-response
            (layer-annotate-arrow 1.5 1.2 2.0 1.7
                                  #:color "blue"
                                  #:arrow-length 12)
            (layer-annotate-text "Dose-dependent response" 1.5 0.75
                                 #:color "blue"
                                 #:size 11
                                 #:hjust 0.5) ; centered below the arrow tail, below baseline
            
            (scale-fill-manual #:values '(("placebo" . "lightgray")
                                          ("drug-A" . "steelblue")
                                          ("drug-B" . "coral"))
                               #:name "Treatment")
            (scale-y-continuous #:label "Response (normalized to baseline)")
            (scale-x-discrete #:label "Dose (mg/kg)")
            (ggtitle "Dose-Response Comparison Across Treatments")
            
            ;; Legend outside on bottom for horizontal layout
            (theme-legend-outside 'bottom)
            (theme-minimal))))
    
    plot))

;;; ========================================================================
;;; PCA Biplot with Repelled Labels
;;; ========================================================================

(define pca-biplot-example
  (let* ((;; Simulate PCA results
          samples '("S1" "S2" "S3" "S4" "S5" "S6" "S7" "S8" "S9" "S10"))
         (groups '("Control" "Control" "Control" "Control" "Control"
                   "Treatment" "Treatment" "Treatment" "Treatment" "Treatment"))
         
         (pca-data
          `((sample . ,samples)
            (group . ,groups)
            (PC1 . (-2.1 -1.8 -2.3 -1.5 -2.0
                    1.9 2.2 1.7 2.5 1.8))
            (PC2 . (0.5 -0.3 0.8 -0.5 0.2
                    -0.4 0.6 -0.8 0.3 -0.2))))
         
         (plot
          (ggplot pca-data (aes #:x 'PC1 #:y 'PC2 #:color 'group)
            ;; Sample points
            (layer-point #:size 5 #:alpha 0.8)
            
            ;; Non-overlapping sample labels
            (geom-text-repel pca-data
                             (aes #:x 'PC1 #:y 'PC2 #:label 'sample)
                             #:size 8
                             #:max-iterations 100)
            
            ;; Origin reference lines
            (annotate-vline 0 #:color "gray" #:linetype 'dotted)
            (annotate-hline 0 #:color "gray" #:linetype 'dotted)
            
            ;; Group separation annotation
            (layer-annotate-segment -1 -1.2 1 -1.2
                                    #:color "black"
                                    #:line-width 1
                                    #:arrow? #t)
            (layer-annotate-segment 1 -1.2 -1 -1.2
                                    #:color "black"
                                    #:line-width 1
                                    #:arrow? #t)
            (layer-annotate-text "Clear separation along PC1" 0 -1.5
                                 #:size 9 #:hjust 0.5)
            
            ;; Variance explained annotation
            (layer-annotate-text "PC1: 68% variance  PC2: 18% variance"
                                 -2.2 1.1
                                 #:size 11
                                 #:hjust 0
                                 #:vjust 1)
            
            (scale-color-manual #:values '(("Control" . "steelblue")
                                           ("Treatment" . "coral"))
                                #:name "Group")
            (scale-x-continuous #:label "PC1 (68% variance)")
            (scale-y-continuous #:label "PC2 (18% variance)")
            (coord-fixed #:ratio 1)
            (ggtitle "Principal Component Analysis - Sample Clustering")
            (theme-minimal)
            (theme-legend-inside 'bottom-left))))
    
    plot))

;;; ========================================================================
;;; Batch Rendering
;;; ========================================================================

(define (render-all-examples)
  "Render all example plots to PNG files"
  (render-plot spike-raster-example
               (make-png-plotter "spike-raster.png" 800 600))
  (render-plot volcano-plot-example
               (make-png-plotter "volcano-plot.png" 1000 800))
  (render-plot calibration-plot-example
               (make-png-plotter "calibration.png" 700 700))
  (render-plot eeg-timefreq-example
               (make-png-plotter "eeg-timefreq.png" 1000 600))
  (render-plot boxplot-comparison-example
               (make-png-plotter "boxplot-comparison.png" 900 700))
  (render-plot pca-biplot-example
               (make-png-plotter "pca-biplot.png" 800 800))
  )

(render-all-examples)
