;;;; gg-primitives-examples.scm
;;;; Examples demonstrating basic drawing primitives

(import scheme
        (chicken base)
        (chicken random)
        gg-primitives
        srfi-1)

;;; ========================================================================
;;; Basic shapes and composition
;;; ========================================================================

(define example-basic-shapes
  ;; basic shape primitives and combine
  (combine
   
   ;; Background
   (rectangle 0 0 800 600 #:fill-color "white")
   
   ;; Filled circle
   (circle 200 200 50 
           #:fill-color "red" )
           ;#:edge-color "navy" 
           ;#:line-width 2)
   
   ;; Line with style
   (line 100 100 700 500 
         #:color "red" 
         #:width 3 
         #:style 'dashed)
   
   ;; Polygon
   (polygon '((400 . 100) (500 . 150) (450 . 250) (350 . 200))
            #:fill-color "lightgreen"
            #:edge-color "darkgreen"
            #:width 1.5)
   
   ;; Text
   (text 400 50 "Primitives Demo"
         #:color "black"
         #:size 12.0)
   
   ))

;; Render to PNG
(define (run-example-basic-shapes)
  (render example-basic-shapes
          (make-png-plotter "example-basic-shapes.png" 800 600)))

;;; ========================================================================
;;; Style combinators
;;; ========================================================================

(define example-style-transform
  ;; style transformation
  (let ((base-circle (circle 0 0 30 #:fill-color "orange")))
    (combine
     ;; Background
     (rectangle 0 0 800 600 #:fill-color "white")
     
     ;; Original at center
     (with-translate 400 300 base-circle)
     
     ;; Translated copies with different styles
     (with-translate 200 300
       (with-pen-color "blue"
         (circle 0 0 30 #:edge-color "blue" #:line-width 2)))
     
     (with-translate 600 300
       (with-line-width 4
         (circle 0 0 30 #:edge-color "red")))
     
     ;; Scaled version
     (with-translate 400 150
       (with-scale 1.5 1.5 base-circle))
     
     ;; Rotated rectangle
     (with-translate 400 450
       (with-rotate (/ 3.14159 4)  ; 45 degrees
         (rectangle -40 -20 80 40
                    #:fill-color "purple"
                    #:edge-color "white"
                    #:line-width 2))))))

(define (run-example-style-transform)
  (render example-style-transform
          (make-png-plotter "example-style-transform.png" 800 600)))

;;; ========================================================================
;;; Functional composition - grid of shapes
;;; ========================================================================

(define (make-grid rows cols cell-width cell-height cell-drawer)
  "Create a grid of identical shapes with functional composition"
  (apply combine
         (append-map
          (lambda (row)
            (map (lambda (col)
                   (with-translate (* col cell-width) 
                                   (* row cell-height)
                     cell-drawer))
                 (iota cols)))
          (iota rows))))

(define example-grid-of-shapes
  ; Grid of shapes demonstrating reusability
  (let ((cell (combine
               (circle 25 25 20 #:fill-color "lightblue")
               (circle 25 25 20 #:edge-color "darkblue" #:line-width 1.5)
               (text-centered 25 25 "S" #:color "navy" #:size 15.0))))
    (combine
     (rectangle 0 0 800 600 #:fill-color "white")
     (make-grid 4 6 130 130 cell))))

(define (run-example-grid-of-shapes)
  (render example-grid-of-shapes
          (make-svg-plotter "example-grid-of-shapes.svg" 800 600)))

;;; ========================================================================
;;; Polyline and curves
;;; ========================================================================

(define (generate-sine-wave x-start x-end steps amplitude)
  "Generate points for a sine wave"
  (let ((dx (/ (- x-end x-start) steps)))
    (map (lambda (i)
           (let ((x (+ x-start (* i dx))))
             (cons x (* amplitude (sin (* 2 3.14159 (/ x 100)))))))
         (iota (+ steps 1)))))

(define example-polylines
  ;; polylines and Bezier curves
  (combine
   ;; Background
   (rectangle 0 0 800 600 #:fill-color "white")
   
   ;; Sine wave
   (polyline (generate-sine-wave 50 750 100 50)
             #:color "blue"
             #:width 2)
   
   ;; Bezier curve
   (bezier 100 500 200 550 600 400 700 500
           #:color "red"
           #:width 2)
   
   ;; Control points for Bezier
   (circle 100 500 5 #:fill-color "red")
   (circle 200 550 5 #:fill-color "red")
   (circle 600 400 5 #:fill-color "red")
   (circle 700 500 5 #:fill-color "red")
   
   ;; Labels
   (text 50 250 "Sine Wave" #:color "blue" #:size 12.0)
   (text 50 450 "Bezier Curve" #:color "red" #:size 12.0)))

(define (run-example-polylines)
  (render example-polylines
          (make-png-plotter "example-polylines.png" 800 600)))

;;; ========================================================================
;;; Simple plot-like visualization
;;; ========================================================================

(define (scatter-plot points x-min x-max y-min y-max 
                      width height margin)
  "Create a simple scatter plot from data points"
  (let* ((plot-width (- width (* 2 margin)))
         (plot-height (- height (* 2 margin)))
         (x-scale (lambda (x) 
                    (+ margin (* plot-width 
                                 (/ (- x x-min) (- x-max x-min))))))
         (y-scale (lambda (y) 
                    (+ margin (* plot-height 
                                 (/ (- y y-min) (- y-max y-min)))))))
    
    (combine
     ;; Background
     (rectangle 0 0 width height #:fill-color "white")
     
     ;; Plot area background
     (rectangle margin margin plot-width plot-height
                #:fill-color "gray95")
     
     ;; Axes
     (line margin margin margin (+ margin plot-height)
           #:color "black" #:width 2)
     (line margin margin (+ margin plot-width) margin
           #:color "black" #:width 2)
     
     ;; Data points
     (apply combine
            (map (lambda (pt)
                   (circle (x-scale (car pt)) 
                           (y-scale (cdr pt))
                           4
                           #:fill-color "steelblue"
                           #:edge-color "navy"))
                 points))
     
     ;; Axis labels
     (text (+ margin (/ plot-width 2)) 10 "X Axis"
           #:color "black" #:size 12.0)
     (text 20 (+ margin (/ plot-height 2)) "Y Axis"
           #:color "black" #:size 12.0))))

(define example-simple-scatter-plot
  ; scatter plot demonstration
  (let ((data '((1 . 2.3) (2 . 4.1) (3 . 3.5) (4 . 5.8) 
                (5 . 5.2) (6 . 7.1) (7 . 6.8) (8 . 8.2))))
    (scatter-plot data 0 10 0 10 800 600 50)))

(define (run-example-simple-scatter-plot)
  (render example-simple-scatter-plot
          (make-png-plotter "example-simple-scatter-plot.png" 800 600)))

;;; ========================================================================
;;; Neuroscience-inspired - Spike raster pattern
;;; ========================================================================

(define (spike-raster trial-count spike-times-fn y-start y-step spike-height)
  "Draw a spike raster plot pattern"
  (apply combine
         (map (lambda (trial)
                (let ((y (+ y-start (* trial y-step)))
                      (spike-times (spike-times-fn trial)))
                  (apply combine
                         (map (lambda (t)
                                (line t (- y (/ spike-height 2))
                                      t (+ y (/ spike-height 2))
                                      #:color "black"
                                      #:width 1))
                              spike-times))))
              (iota trial-count))))

(define example-spike-raster
  ; Neiron spike raster
  (let ((random-spikes 
         (lambda (trial)
           ;; Generate pseudo-random spike times
           (filter (lambda (t) (< (pseudo-random-integer 100) 30))
                   (iota 100)))))
    (combine
     ;; Background
     (rectangle 0 0 800 600 #:fill-color "white")
     
     ;; Stimulus period marker
     (rectangle 200 50 100 500 
                #:fill-color "yellow" 
                #:edge-color "none")
     
     ;; Spike rasters
     (with-translate 50 50
       (spike-raster 20 random-spikes 0 25 20))
     
     ;; Axis
     (line 50 575 750 575 #:color "black" #:width 2)
     
     ;; Labels
     (text 400 590 "Time (ms)" #:color "black" #:size 12.0)
     (text 20 300 "Trials" #:color "black" #:size 12.0)
     (text 300 20 "Stimulus" #:color "orange" #:size 10.0))))

(define (run-example-spike-raster)
  (render example-spike-raster
          (make-png-plotter "example-spike-raster.png" 800 600)))

;;; ========================================================================
;;; Complex composition - Dashboard layout
;;; ========================================================================

(define (panel x y width height title drawer)
  "Create a titled panel containing a drawer"
  (combine
   ;; Panel background
   (rectangle x y width height
              #:fill-color "white"
              #:edge-color "gray"
              #:line-width 1)
   ;; Title bar
   (rectangle x (+ y height -25) width 25
              #:fill-color "lightgray")
   ;; Title text
   (text (+ x 80) (+ y height -10) title
         #:color "black"
         #:size 12.0)
   ;; Content with proper translation
   (with-translate x y drawer)))

(define example-multi-panel
  ; Multi-panel dashboard layout
  (combine
   ;; Background
   (rectangle 0 0 1200 800 #:fill-color "whitesmoke")
   
   ;; Panel 1: Circle
   (panel 20 20 350 350 "Panel 1: Geometry"
          (circle 175 175 100 
                  #:fill-color "coral"
                  #:edge-color "darkred"
                  #:line-width 2))
   
   ;; Panel 2: Line chart
   (panel 390 20 350 350 "Panel 2: Time Series"
          (polyline (generate-sine-wave 20 330 50 100)
                    #:color "steelblue"
                    #:width 2))
   
   ;; Panel 3: Grid pattern
   (panel 760 20 420 350 "Panel 3: Pattern"
          (make-grid 4 6 70 70
                     (circle 35 35 25
                             #:fill-color "lightgreen"
                             #:edge-color "darkgreen")))
   
   ;; Panel 4: Text and labels
   (panel 20 390 560 390 "Panel 4: Annotations"
          (combine
           (text 120 200 "Large text label"
                 #:color "navy"
                 #:size 25.0)
           (text 80 100 "Smaller annotation"
                 #:color "gray"
                 #:size 12.0)))))

(define (run-example-multi-panel)
  (render example-multi-panel
          (make-png-plotter "example-multi-panel.png" 1200 800)))

;;; ========================================================================
;;; Run all examples
;;; ========================================================================

(define (run-all-examples)
  (run-example-basic-shapes)
  (run-example-style-transform)
  (run-example-grid-of-shapes)
  (run-example-polylines)
  (run-example-simple-scatter-plot)
  (run-example-spike-raster)
  (run-example-multi-panel)
  )

(run-all-examples)
