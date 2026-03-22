;;;; Complete examples demonstrating Grammar of Graphics API

(import scheme
        (chicken base)
        (chicken format)
        (chicken string)
        gg-plot
        gg-scales
        gg-aes
        gg-backend-cairo)

;;; ========================================================================
;;; Example 1: Basic Time Series with Layers
;;; ========================================================================

(define example-1-basic-time-series
  (lambda ()
    "Demonstrate layered composition with points, lines, and areas"
    
    (define data
      '((time . (0 10 20 30 40 50 60 70 80 90 100))
        (value . (1.2 1.5 2.3 1.8 1.4 1.9 2.5 2.1 1.7 2.0 2.3))
        (upper . (1.5 1.8 2.6 2.1 1.7 2.2 2.8 2.4 2.0 2.3 2.6))
        (lower . (0.9 1.2 2.0 1.5 1.1 1.6 2.2 1.8 1.4 1.7 2.0))))
    
    (define plot
      (ggplot data (aes #:x 'time #:y 'value)
        ;; Layer 1: Confidence band (area)
        (layer 'area 
               #:mapping (aes #:ymin 'lower #:ymax 'upper)
               #:fill "lightblue"
               #:alpha 0.3)
        
        ;; Layer 2: Main line
        (layer 'line 
               #:color "steelblue"
               #:width 2)
        
        ;; Layer 3: Data points
        (layer 'point 
               #:size 5
               #:color "darkblue")
        
        ;; Scales
        (scale-x-continuous #:name "Time (seconds)")
        (scale-y-continuous #:name "Response (mV)")
        
        ;; Theme and labels
        (theme-minimal #:base-size 12.0)
        (labs #:title "Time Series with Confidence Band"
              #:subtitle "Example of layered composition")))
    
    (ggsave plot "ex1-timeseries.png" #:width 800 #:height 600)
    (display "Example 1 complete: ex1-timeseries.png\n")))

;;; ========================================================================
;;; Example 2: Multi-Series Line Plot with Color Mapping
;;; ========================================================================

(define example-2-multi-series
  (lambda ()
    "Multiple time series distinguished by color"
    
    (define data
      '((time . (0 10 20 30 40 50 60 0 10 20 30 40 50 60))
        (value . (1.2 2.3 1.8 2.5 2.1 1.9 2.4 0.8 1.5 1.2 1.8 1.6 1.4 1.7))
        (condition . ("Control" "Control" "Control" "Control" "Control" "Control" "Control"
                      "Treatment" "Treatment" "Treatment" "Treatment" "Treatment" "Treatment" "Treatment"))))
    
    (define plot
      (ggplot data (aes #:x 'time #:y 'value #:color 'condition)
        ;; Lines distinguished by color
        (layer 'line #:width 2)
        (layer 'point #:size 4)
        
        ;; Manual color scale
        (scale-x-continuous #:name "Time (ms)")
        (scale-y-continuous #:name "Response")
        (scale-color-manual #:values '(("Control" . "blue")
                                       ("Treatment" . "red")))
        
        ;; Theme
        (theme-classic #:base-size 11)
        (labs #:title "Treatment vs Control"
              #:subtitle "Two experimental conditions")))
    
    (ggsave plot "ex2-multiseries.png" #:width 800 #:height 600)
    (display "Example 2 complete: ex2-multiseries.png\n")))

;;; ========================================================================
;;; Example 3: Bar Chart with Categorical Data
;;; ========================================================================

(define example-3-bar-chart
  (lambda ()
    "Bar chart demonstrating discrete scales"
    
    (define data
      '((region . ("V1" "V2" "V4" "MT" "IT"))
        (firing-rate . (12.5 8.3 15.7 22.1 9.4))
        (condition . ("Baseline" "Baseline" "Baseline" "Baseline" "Baseline"))))
    
    (define plot
      (ggplot data (aes #:x 'region #:y 'firing-rate)
        ;; Bar geometry
        (layer 'bar
               #:fill "steelblue"
               #:width 0.7)
        
        ;; Scales
        (scale-x-discrete #:name "Brain Region")
        (scale-y-continuous #:name "Firing Rate (Hz)"
                            #:limits '(0 25))
        
        ;; Theme
        (theme-bw)
        (labs #:title "Firing Rates Across Brain Regions"
              #:subtitle "Mean baseline activity")))
    
    (ggsave plot "ex3-bars.png" #:width 700 #:height 500)
    (display "Example 3 complete: ex3-bars.png\n")))

;;; ========================================================================
;;; Example 4: Spike Raster Plot (Neuroscience)
;;; ========================================================================

(define example-4-spike-raster
  (lambda ()
    "Event plot for spike train visualization"
    
    (define spike-data
      '((trial . (0 1 2 3 4 5 6 7 8 9))
        (spikes . ((12 25 38 55 67) 
                   (15 28 42 58 71)
                   (10 23 40 52 65)
                   (18 30 45 60 73)
                   (13 27 41 56 68)
                   (16 29 43 59 72)
                   (11 24 39 54 66)
                   (14 26 44 57 70)
                   (17 31 46 61 74)
                   (9 22 37 53 64)))
        (condition . ("Pre" "Pre" "Pre" "Pre" "Pre"
                      "Post" "Post" "Post" "Post" "Post"))))
    
    (define plot
      (ggplot spike-data (aes #:x 'spikes #:y 'trial #:color 'condition)
        ;; Event lines
        (layer 'eventplot 
               #:line-length 0.7
               #:width 4)
        
        ;; Stimulus marker
        (layer 'vline 
               #:xintercept 20 
               #:color "red" 
               #:style 'dashed
               #:width 1.5)
        
        ;; Scales
        (scale-x-continuous #:name "Time (ms)" 
                           #:limits '(0 80))
        (scale-y-continuous #:name "Trial #")
        (scale-color-manual #:values '(("Pre" . "black")
                                       ("Post" . "blue")))
        
        ;; Theme
        (theme-classic #:base-size 11)
        (labs #:title "Spike Raster Plot"
              #:subtitle "Pre vs Post stimulus"
              #:caption "Red line indicates stimulus onset")))
    
    (ggsave plot "ex4-raster.png" #:width 800 #:height 600)
    (display "Example 4 complete: ex4-raster.png\n")))

;;; ========================================================================
;;; Example 5: Faceted Plot (Small Multiples)
;;; ========================================================================

(define example-5-faceted
  (lambda ()
    "Faceted plot demonstrating small multiples"
    
    (define data
      '((time . (0 10 20 30 0 10 20 30 0 10 20 30))
        (power . (8 12 15 11 5 9 14 10 12 18 22 17))
        (frequency . ("Alpha" "Alpha" "Alpha" "Alpha"
                      "Beta" "Beta" "Beta" "Beta"
                      "Gamma" "Gamma" "Gamma" "Gamma"))
        (region . ("V1" "V1" "V1" "V1" "V1" "V1" "V1" "V1" "V1" "V1" "V1" "V1"))))
    
    (define plot
      (ggplot data (aes #:x 'time #:y 'power)
        ;; Geometry
        (layer 'line #:color "steelblue" #:width 2)
        (layer 'point #:size 4 #:color "darkblue")
        
        ;; Faceting by frequency band
        (facet-wrap 'frequency 
                    #:ncol 3 
                    #:scales "free_y")
        
        ;; Scales
        (scale-x-continuous #:name "Time (s)")
        (scale-y-continuous #:name "Power (dB)")
        
        ;; Theme
        (theme-bw #:base-size 10)
        (labs #:title "Power Across Frequency Bands"
              #:subtitle "Region V1")))
    
    (ggsave plot "ex5-faceted.png" #:width 1200 #:height 400)
    (display "Example 5 complete: ex5-faceted.png\n")))

;;; ========================================================================
;;; Example 6: Logarithmic Scale
;;; ========================================================================

(define example-6-log-scale
  (lambda ()
    "Demonstrate logarithmic y-axis"
    
    (define data
      '((concentration . (0.1 1 10 100 1000))
        (response . (2.1 8.5 42.3 201.7 987.2))))
    
    (define plot
      (ggplot data (aes #:x 'concentration #:y 'response)
        ;; Points and line
        (layer 'point #:size 6 #:color "coral")
        (layer 'line #:width 2 #:color "coral")
        
        ;; Logarithmic scales
        (scale-x-log #:name "Concentration (μM)" #:base 10)
        (scale-y-log #:name "Response (% baseline)" #:base 10)
        
        ;; Theme
        (theme-minimal #:base-size 12)
        (labs #:title "Dose-Response Curve"
              #:subtitle "Log-log plot")))
    
    (ggsave plot "ex6-logscale.png" #:width 700 #:height 600)
    (display "Example 6 complete: ex6-logscale.png\n")))

;;; ========================================================================
;;; Example 7: Plot Specification Serialization
;;; ========================================================================

(define example-7-serialization
  (lambda ()
    "Demonstrate saving and loading plot specifications"
    
    (define data
      '((x . (1 2 3 4 5))
        (y . (2.1 4.3 3.8 5.2 4.9))))
    
    ;; Create plot
    (define original-plot
      (ggplot data (aes #:x 'x #:y 'y)
        (layer 'point #:size 6)
        (layer 'line #:width 2 #:color "steelblue")
        (scale-x-continuous #:name "X Variable")
        (scale-y-continuous #:name "Y Variable")
        (theme-classic)
        (labs #:title "Serialization Example")))
    
    ;; Convert to s-expression
    (define plot-sexp (plot->sexp original-plot))
    
    ;; Save to file
    (with-output-to-file "plot-spec.scm"
      (lambda ()
        (write plot-sexp)
        (newline)))
    
    ;; Load from file
    (define loaded-sexp
      (with-input-from-file "plot-spec.scm" read))
    
    ;; Convert back to plot
    (define restored-plot (sexp->plot loaded-sexp))
    
    ;; Render restored plot
    (ggsave restored-plot "ex7-serialized.png" #:width 700 #:height 500)
    (display "Example 7 complete: ex7-serialized.png\n")
    (display "  Plot specification saved to: plot-spec.scm\n")))

;;; ========================================================================
;;; Example 8: Programmatic Plot Generation
;;; ========================================================================

(define example-8-programmatic
  (lambda ()
    "Generate multiple plots programmatically"
    
    (define base-data
      '((time . (0 10 20 30 40 50))
        (var1 . (1.2 2.3 1.8 2.5 2.1 1.9))
        (var2 . (0.8 1.5 1.2 1.8 1.6 1.4))
        (var3 . (2.1 3.2 2.5 3.4 2.9 2.7))))
    
    ;; Function to create comparison plot for given variables
    (define (make-comparison-plot vars)
      (apply ggplot base-data (aes #:x 'time)
        (append
          ;; Create a line layer for each variable
          (map (lambda (var)
                 (layer 'line 
                        #:mapping (aes #:y var)
                        #:width 2))
               vars)
          ;; Add common elements
          (list
            (scale-x-continuous #:name "Time (s)")
            (scale-y-continuous #:name "Value")
            (theme-minimal)
            (labs #:title "Variable Comparison"
                  #:subtitle (format "Variables: ~a" 
                                    (string-intersperse 
                                      (map symbol->string vars) ", ")))))))
    
    ;; Generate plot with selected variables
    (define plot (make-comparison-plot '(var1 var2 var3)))
    
    (ggsave plot "ex8-programmatic.png" #:width 800 #:height 600)
    (display "Example 8 complete: ex8-programmatic.png\n")))

;;; ========================================================================
;;; Example 9: Gradient Color Scale
;;; ========================================================================

(define example-9-gradient
  (lambda ()
    "Demonstrate gradient color mapping"
    
    (define data
      '((x . (1 2 3 4 5 1 2 3 4 5 1 2 3 4 5))
        (y . (1 1 1 1 1 2 2 2 2 2 3 3 3 3 3))
        (value . (2.1 4.5 6.8 8.2 9.5
                 3.4 5.9 7.3 8.8 9.9
                 4.2 6.1 7.9 9.1 9.8))))
    
    (define plot
      (ggplot data (aes #:x 'x #:y 'y #:color 'value)
        ;; Points sized and colored by value
        (layer 'point #:size 15)
        
        ;; Gradient color scale
        (scale-color-gradient #:low "blue" 
                              #:high "red"
                              #:limits '(0 10))
        
        ;; Scales
        (scale-x-continuous #:name "X Position")
        (scale-y-continuous #:name "Y Position")
        
        ;; Theme
        (theme-void)
        (labs #:title "Gradient Color Mapping"
              #:subtitle "Blue (low) to Red (high)")))
    
    (ggsave plot "ex9-gradient.png" #:width 600 #:height 600)
    (display "Example 9 complete: ex9-gradient.png\n")))

;;; ========================================================================
;;; Example 10: Complete Neuroscience Analysis
;;; ========================================================================

(define example-10-complete-analysis
  (lambda ()
    "Comprehensive example: LFP with spike overlay and annotations"
    
    (define lfp-data
      '((time . (0 5 10 15 20 25 30 35 40 45 50 55 60))
        (lfp . (0.1 0.3 0.8 0.4 -0.2 -0.5 -0.3 0.1 0.4 0.6 0.3 0.2 0.0))
        (spike-rate . (2 5 12 8 3 1 2 4 9 11 7 5 3))))
    
    (define plot
      (ggplot lfp-data (aes #:x 'time)
        ;; Background: stimulus period
        (layer 'rect
               #:xmin 10 #:xmax 30 
               #:ymin -1 #:ymax 1
               #:fill "yellow"
               #:alpha 0.2)
        
        ;; Layer 1: LFP trace
        (layer 'area
               #:mapping (aes #:y 'lfp)
               #:fill "steelblue"
               #:alpha 0.3)
        (layer 'line
               #:mapping (aes #:y 'lfp)
               #:color "darkblue"
               #:width 2)
        
        ;; Layer 2: Spike rate (on secondary axis - future feature)
        ;; For now, normalize to same scale
        (layer 'line
               #:mapping (aes #:y 'spike-rate)
               #:color "coral"
               #:width 1.5
               #:style 'dashed)
        
        ;; Annotations: stimulus markers
        (layer 'vline #:xintercept 10 #:color "red" #:style 'solid)
        (layer 'vline #:xintercept 30 #:color "red" #:style 'solid)
        
        ;; Scales
        (scale-x-continuous #:name "Time (ms)"
                           #:breaks '(0 10 20 30 40 50 60))
        (scale-y-continuous #:name "LFP Amplitude (mV)")
        
        ;; Theme
        (theme-minimal #:base-size 11)
        (labs #:title "Neural Recording Analysis"
              #:subtitle "LFP with spike rate overlay"
              #:caption "Yellow region indicates stimulus period")))
    
    (ggsave plot "ex10-complete.png" #:width 900 #:height 600)
    (display "Example 10 complete: ex10-complete.png\n")))

;;; ========================================================================
;;; Example 11: Using Different Output Formats
;;; ========================================================================

(define example-11-output-formats
  (lambda ()
    "Demonstrate SVG and PostScript output"
    
    (define data
      '((x . (1 2 3 4 5))
        (y . (2 4 3 5 4))))
    
    (define plot
      (ggplot data (aes #:x 'x #:y 'y)
        (layer 'point #:size 6 #:color "steelblue")
        (layer 'line #:width 2 #:color "steelblue")
        (scale-x-continuous #:name "X")
        (scale-y-continuous #:name "Y")
        (theme-minimal)
        (labs #:title "Multi-Format Output")))
    
    ;; SVG output
    (render-plot plot (make-cairo-svg-backend "ex11-output.svg" 600 400))

    ;; PostScript output
    (render-plot plot (make-cairo-ps-backend "ex11-output.ps" 600 400))

    ;; PNG output (high resolution: 4× pixel dimensions)
    (ggsave plot "ex11-output-hires.png" #:width 2400 #:height 1600)

    (display "Example 11 complete: SVG, PS, and high-res PNG outputs\n")))

;;; ========================================================================
;;; Run All Examples
;;; ========================================================================

(define (run-all-examples)
  (example-1-basic-time-series)
  (example-2-multi-series)
  (example-3-bar-chart)
  (example-4-spike-raster)
  (example-5-faceted)
  (example-6-log-scale)
  (example-7-serialization)
  (example-8-programmatic)
  (example-9-gradient)
  (example-10-complete-analysis)
  (example-11-output-formats)
  )

(run-all-examples)
