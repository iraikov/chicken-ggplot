;;;; gg-tier2-examples.scm
;;;; Examples demonstrating Tier 2: Scales and Axes

(import scheme
        (chicken base)
        (chicken format)
        (chicken random)
        gg-primitives
        gg-scales
        gg-guides
        srfi-1)

;;; ========================================================================
;;; Linear scale with axes
;;; ========================================================================

(define example-linear-scale
  (let* (;; Create scales
         (x-scale (make-scale-linear))
         (y-scale (make-scale-linear))
         
         ;; Sample data
         (x-data '(0 1 2 3 4 5 6 7 8 9 10))
         (y-data '(1.2 2.3 1.8 3.5 4.2 3.8 5.1 4.9 6.2 5.8 7.1))
         
         ;; Train scales on data
         (_ (begin
              (scale-train! x-scale x-data)
              (scale-train! y-scale y-data)))
         
         ;; Set visual ranges (leaving margins for axes)
         (_ (begin
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale #:label "X Variable" #:tick-count 6))
         (y-axis (make-axis-left y-scale #:label "Y Variable" #:tick-count 6))
         
         ;; Plot data points
         (points (apply combine
                        (map (lambda (x y)
                               (circle (scale-map x-scale x)
                                       (scale-map y-scale y)
                                       5
                                       #:fill-color "steelblue"
                                       #:edge-color "navy"))
                             x-data y-data))))
    
    (combine
     ;; Background
     (rectangle 0 0 800 600 #:fill-color "white")
     
     ;; Plot area background
     (rectangle 80 80 640 440 #:fill-color "gray95")
     
     ;; Axes
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     
     ;; Data
     points
     
     ;; Title
     (text 400 560 "Linear Scale Example"
           #:color "black" #:size 15.0))))

(define (run-example-linear-scale)
  (render example-linear-scale
          (make-png-plotter "example-linear-scale.png" 800 600)))

;;; ========================================================================
;;; Logarithmic scale
;;; ========================================================================

(define example-log-scale
  (let* (;; Create scales - x linear, y logarithmic
         (x-scale (make-scale-linear))
         (y-scale (make-scale-log #:base 10))
         
         ;; Sample data (exponential growth)
         (x-data '(0 1 2 3 4 5 6 7 8))
         (y-data '(1 2 4 8 16 32 64 128 256))
         
         ;; Train and set ranges
         (_ (begin
              (scale-train! x-scale x-data)
              (scale-train! y-scale y-data)
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale #:label "Time" #:tick-count 9))
         (y-axis (make-axis-left y-scale #:label "Population (log scale)" #:tick-count 4))
         
         ;; Plot line
         (data-line (polyline 
                     (map (lambda (x y)
                            (cons (scale-map x-scale x)
                                  (scale-map y-scale y)))
                          x-data y-data)
                     #:color "red"
                     #:width 2)))
    
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     (rectangle 80 80 640 440 #:fill-color "gray95")
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     data-line
     (text 400 560 "Logarithmic Scale Example"
           #:color "black" #:size 15.0))))

(define (run-example-log-scale)
  (render example-log-scale
          (make-png-plotter "example-log-scale.png" 800 600)))

;;; ========================================================================
;;; Ordinal scale (categorical data)
;;; ========================================================================

(define example-ordinal-scale
  (let* (;; Create scales
         (x-scale (make-scale-band))
         (y-scale (make-scale-linear))
         
         ;; Categorical data
         (categories '("A" "B" "C" "D" "E"))
         (values '(23 45 12 67 34))
         
         ;; Train and set ranges
         (_ (begin
              (scale-train! x-scale categories)
              (scale-train! y-scale values)
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale #:label "Category"))
         (y-axis (make-axis-left y-scale #:label "Value" #:tick-count 5))
         
         ;; Create bars
         (bars (apply combine
                      (map (lambda (cat val)
                             (let ((x (scale-map x-scale cat))
                                   (y0 (scale-map y-scale 12))
                                   (y1 (scale-map y-scale val))
                                   (w 50))
                               (rectangle (- x (/ w 2)) y0 w (- y1 y0)
                                         #:fill-color "coral"
                                         #:edge-color "darkred"
                                         #:line-width 1)))
                           categories values))))
    
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     (rectangle 80 80 640 440 #:fill-color "gray95")
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     bars
     (text 400 560 "Ordinal (Categorical) Scale Example"
           #:color "black" #:size 15.0))))

(define (run-example-ordinal-scale)
  (render example-ordinal-scale
          (make-png-plotter "example-ordinal-scale.png" 800 600)))

;;; ========================================================================
;;; Multiple axes (dual y-axis)
;;; ========================================================================

(define example-multiple-axes
  (let* (;; Shared x-scale
         (x-scale (make-scale-linear))
         ;; Two different y-scales
         (y1-scale (make-scale-linear))
         (y2-scale (make-scale-linear))
         
         ;; Data
         (x-data '(0 1 2 3 4 5 6 7 8 9 10))
         (y1-data '(10 12 15 14 18 20 19 23 25 24 28))  ; Temperature
         (y2-data '(30 28 32 25 35 38 40 36 42 45 43))  ; Humidity
         
         ;; Train and set ranges
         (_ (begin
              (scale-train! x-scale x-data)
              (scale-train! y1-scale y1-data)
              (scale-train! y2-scale y2-data)
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y1-scale '(80 . 520))
              (scale-set-range! y2-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale #:label "Time (hours)"))
         (y1-axis (make-axis-left y1-scale #:label "Temperature (°C)"))
         (y2-axis (make-axis-right y2-scale #:label "Humidity (%)"))
         
         ;; Plot lines
         (temp-line (polyline 
                     (map (lambda (x y)
                            (cons (scale-map x-scale x)
                                  (scale-map y1-scale y)))
                          x-data y1-data)
                     #:color "red"
                     #:width 2))
         
         (humidity-line (polyline 
                         (map (lambda (x y)
                                (cons (scale-map x-scale x)
                                      (scale-map y2-scale y)))
                              x-data y2-data)
                         #:color "blue"
                         #:width 2)))
    
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     (rectangle 80 80 640 440 #:fill-color "gray95")
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y1-axis))
     (with-translate 720 0 (axis-drawer y2-axis))
     temp-line
     humidity-line
     ;; Legend
     (text 100 550 "Temperature" #:color "red" #:size 10.0)
     (text 250 550 "Humidity" #:color "blue" #:size 10.0)
     (text 400 570 "Dual Y-Axis Example"
           #:color "black" #:size 15.0))))

(define (run-example-multiple-axes)
  (render example-multiple-axes
          (make-png-plotter "example-multiple-axes.png" 800 600)))

;;; ========================================================================
;;; Neuroscience application - firing rate over time
;;; ========================================================================

(define example-neuroscience-plots
  (let* (;; Create scales
         (x-scale (make-scale-linear))
         (y-scale (make-scale-linear))
         
         ;; Simulated firing rate data
         (time-points (iota 100))
         (firing-rates 
          (map (lambda (t)
                 ;; Baseline + stimulus response
                 (let ((baseline 5)
                       (stimulus-start 30)
                       (stimulus-end 60))
                   (if (and (>= t stimulus-start) (<= t stimulus-end))
                       (+ baseline (* 20 (sin (* 0.2 (- t stimulus-start)))))
                       (+ baseline (pseudo-random-real)))))
               time-points))
         
         ;; Train and set ranges
         (_ (begin
              (scale-train! x-scale time-points)
              (scale-train! y-scale firing-rates)
              ;; Extend y domain for visual clarity
              (scale-set-domain! y-scale (extend-domain (scale-domain y-scale) 0.1))
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale #:label "Time (ms)" #:tick-count 6))
         (y-axis (make-axis-left y-scale #:label "Firing Rate (Hz)" #:tick-count 6))
         
         ;; Stimulus period marker
         (stimulus-marker
          (rectangle (scale-map x-scale 30) 
                     (scale-map y-scale (car (scale-domain y-scale)))
                     (- (scale-map x-scale 60) (scale-map x-scale 30))
                     (- (scale-map y-scale (cdr (scale-domain y-scale)))
                        (scale-map y-scale (car (scale-domain y-scale))))
                     #:fill-color "yellow"
                     #:edge-color "none"))
         
         ;; Plot line
         (data-line (polyline 
                     (map (lambda (t fr)
                            (cons (scale-map x-scale t)
                                  (scale-map y-scale fr)))
                          time-points firing-rates)
                     #:color "darkblue"
                     #:width 2)))
    
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     (rectangle 80 80 640 440 #:fill-color "white")
     stimulus-marker
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     data-line
     (text 400 560 "Neural Firing Rate During Stimulus"
           #:color "black" #:size 15.0)
     (text 300 540 "Stimulus Period" #:color "orange" #:size 10.0))))

(define (run-example-neuroscience-plots)
  (render example-neuroscience-plots
          (make-png-plotter "example-neuroscience-plots.png" 800 600)))

;;; ========================================================================
;;; Grid with scales - heatmap-like visualization
;;; ========================================================================

(define example-simple-heatmap
  (let* (;; Create scales for grid positioning
         (x-scale (make-scale-band))
         (y-scale (make-scale-band))
         (color-scale (make-scale-linear))
         
         ;; Data: 5x5 grid
         (rows '("A" "B" "C" "D" "E"))
         (cols '("1" "2" "3" "4" "5"))
         (values '#(#(0.1 0.3 0.5 0.7 0.9)
                    #(0.2 0.4 0.6 0.8 1.0)
                    #(0.15 0.35 0.55 0.75 0.95)
                    #(0.25 0.45 0.65 0.85 0.95)
                    #(0.05 0.25 0.45 0.65 0.85)))
         
         ;; Train scales
         (_ (begin
              (scale-train! x-scale cols)
              (scale-train! y-scale rows)
              (scale-train! color-scale (apply append (map vector->list (vector->list values))))
              (scale-set-range! x-scale '(80 . 720))
              (scale-set-range! y-scale '(80 . 520))))
         
         ;; Create axes
         (x-axis (make-axis-bottom x-scale))
         (y-axis (make-axis-left y-scale))
         
         ;; Create heatmap cells
         (cells (apply combine
                       (append-map
                        (lambda (row-name row-idx)
                          (map (lambda (col-name col-idx)
                                 (let* ((val (vector-ref (vector-ref values row-idx) col-idx))
                                        (x (scale-map x-scale col-name))
                                        (y (scale-map y-scale row-name))
                                        ;; Grayscale based on value
                                        (gray (inexact->exact (floor (* 255 (- 1 val)))))
                                        (color (format "#~X~X~X" gray gray gray)))
                                   (rectangle (- x 60) (- y 40) 120 80
                                             #:fill-color color
                                             #:edge-color "white"
                                             #:line-width 2)))
                               cols
                               (iota (length cols))))
                        rows
                        (iota (length rows))))))
    
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     cells
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     (text 400 560 "Heatmap Example with Band Scales"
           #:color "black" #:size 15.0))))

(define (run-example-simple-heatmap)
  (render example-simple-heatmap
          (make-png-plotter "example-simple-heatmap.png" 800 600)))

;;; ========================================================================
;;; Run all examples
;;; ========================================================================

(define (run-all-examples)
  (run-example-linear-scale)
  (run-example-log-scale)
  (run-example-ordinal-scale)
  (run-example-multiple-axes)
  (run-example-neuroscience-plots)
  (run-example-simple-heatmap)
  )

(run-all-examples)
