;;;; gg-tier2-examples.scm
;;;; Examples demonstrating Tier 2: Scales and Axes

(import scheme
        (chicken base)
        (chicken format)
        (chicken random)
        gg-primitives-vge
        gg-vge
        gg-backend-cairo
        gg-scales
        gg-guides
        srfi-1)

;;; Local render helper
(define (render-to-png drawer filename width height)
  (let* ((backend (make-cairo-png-backend filename width height))
         (vge     (make-vge)))
    (render-drawer drawer vge)
    (vge-render! vge backend)))

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
                               (combine
                                 (with-fill-color "steelblue"
                                   (filled-circle-drawer (scale-map x-scale x)
                                                         (scale-map y-scale y) 5))
                                 (with-pen-color "navy"
                                   (circle-drawer (scale-map x-scale x)
                                                  (scale-map y-scale y) 5))))
                             x-data y-data))))
    
    (combine
     ;; Background
     (with-fill-color "white"  (filled-rect-drawer 0 0 800 600))

     ;; Plot area background
     (with-fill-color "gray95" (filled-rect-drawer 80 80 640 440))
     
     ;; Axes
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     
     ;; Data
     points
     
     ;; Title
     (with-pen-color "black" (with-font "sans" 15.0 'normal 'normal
       (text-drawer 400 560 "Linear Scale Example")))))
  )

(define (run-example-linear-scale)
  (render-to-png example-linear-scale "example-linear-scale.png" 800 600))

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
         (data-line (with-pen-color "red"
                      (with-line-width 2
                        (polyline-drawer
                          (map (lambda (x y)
                                 (cons (scale-map x-scale x)
                                       (scale-map y-scale y)))
                               x-data y-data))))))
    
    (combine
     (with-fill-color "white"  (filled-rect-drawer 0 0 800 600))
     (with-fill-color "gray95" (filled-rect-drawer 80 80 640 440))
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     data-line
     (with-pen-color "black" (with-font "sans" 15.0 'normal 'normal
       (text-drawer 400 560 "Logarithmic Scale Example")))))
  )

(define (run-example-log-scale)
  (render-to-png example-log-scale "example-log-scale.png" 800 600))

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
                               (with-line-width 1
                                 (filled-rect+border-drawer
                                   (- x (/ w 2)) y0 w (- y1 y0)
                                   "coral" "darkred"))))
                           categories values))))
    
    (combine
     (with-fill-color "white"  (filled-rect-drawer 0 0 800 600))
     (with-fill-color "gray95" (filled-rect-drawer 80 80 640 440))
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     bars
     (with-pen-color "black" (with-font "sans" 15.0 'normal 'normal
       (text-drawer 400 560 "Ordinal (Categorical) Scale Example")))))
)
(define (run-example-ordinal-scale)
  (render-to-png example-ordinal-scale "example-ordinal-scale.png" 800 600))

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
         (temp-line
          (with-pen-color "red" (with-line-width 2
            (polyline-drawer
              (map (lambda (x y)
                     (cons (scale-map x-scale x) (scale-map y1-scale y)))
                   x-data y1-data)))))

         (humidity-line
          (with-pen-color "blue" (with-line-width 2
            (polyline-drawer
              (map (lambda (x y)
                     (cons (scale-map x-scale x) (scale-map y2-scale y)))
                   x-data y2-data))))))
    
    (combine
     (with-fill-color "white"  (filled-rect-drawer 0 0 800 600))
     (with-fill-color "gray95" (filled-rect-drawer 80 80 640 440))
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y1-axis))
     (with-translate 720 0 (axis-drawer y2-axis))
     temp-line
     humidity-line
     ;; Legend
     (with-pen-color "red"   (with-font "sans" 10.0 'normal 'normal (text-drawer 100 550 "Temperature")))
     (with-pen-color "blue"  (with-font "sans" 10.0 'normal 'normal (text-drawer 250 550 "Humidity")))
     (with-pen-color "black" (with-font "sans" 15.0 'normal 'normal (text-drawer 400 570 "Dual Y-Axis Example")))))
)
(define (run-example-multiple-axes)
  (render-to-png example-multiple-axes "example-multiple-axes.png" 800 600))

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
          (with-fill-color "yellow"
            (filled-rect-drawer
              (scale-map x-scale 30)
              (scale-map y-scale (car (scale-domain y-scale)))
              (- (scale-map x-scale 60) (scale-map x-scale 30))
              (- (scale-map y-scale (cdr (scale-domain y-scale)))
                 (scale-map y-scale (car (scale-domain y-scale)))))))

         ;; Plot line
         (data-line
          (with-pen-color "darkblue" (with-line-width 2
            (polyline-drawer
              (map (lambda (t fr)
                     (cons (scale-map x-scale t) (scale-map y-scale fr)))
                   time-points firing-rates))))))
    
    (combine
     (with-fill-color "white" (filled-rect-drawer 0 0 800 600))
     (with-fill-color "white" (filled-rect-drawer 80 80 640 440))
     stimulus-marker
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     data-line
     (with-pen-color "black"  (with-font "sans" 15.0 'normal 'normal (text-drawer 400 560 "Neural Firing Rate During Stimulus")))
     (with-pen-color "orange" (with-font "sans" 10.0 'normal 'normal (text-drawer 300 540 "Stimulus Period")))))
  )

(define (run-example-neuroscience-plots)
  (render-to-png example-neuroscience-plots "example-neuroscience-plots.png" 800 600))

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
                                        (hex2 (lambda (n) (let ((s (number->string n 16)))
                                                           (if (< n 16) (string-append "0" s) s))))
                                        (color (string-append "#" (hex2 gray) (hex2 gray) (hex2 gray))))
                                   (with-line-width 2
                                     (filled-rect+border-drawer
                                       (- x 60) (- y 40) 120 80
                                       color "white"))))
                               cols
                               (iota (length cols))))
                        rows
                        (iota (length rows))))))
    
    (combine
     (with-fill-color "white" (filled-rect-drawer 0 0 800 600))
     cells
     (with-translate 0 80 (axis-drawer x-axis))
     (with-translate 80 0 (axis-drawer y-axis))
     (with-pen-color "black" (with-font "sans" 15.0 'normal 'normal
       (text-drawer 400 560 "Heatmap Example with Band Scales")))))
  )

(define (run-example-simple-heatmap)
  (render-to-png example-simple-heatmap "example-simple-heatmap.png" 800 600))

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
