;;;; gg-primitives-examples.scm
;;;; Examples demonstrating VGE-based drawing primitives (gg-primitives-vge)

(import scheme
        (chicken base)
        (chicken random)
        gg-primitives-vge
        gg-vge
        gg-backend-cairo
        gg-backend    ; for path:move-to, path:curve-to, halign/center, valign/center
        srfi-1)

;;; Local render helpers
(define (render-to-png drawer filename width height)
  (let* ((backend (make-cairo-png-backend filename width height))
         (vge     (make-vge)))
    (render-drawer drawer vge)
    (vge-render! vge backend)))

(define (render-to-svg drawer filename width height)
  (let* ((backend (make-cairo-svg-backend filename width height))
         (vge     (make-vge)))
    (render-drawer drawer vge)
    (vge-render! vge backend)))

;;; ========================================================================
;;; Basic shapes and composition
;;; ========================================================================

(define example-basic-shapes
  ;; basic shape primitives and combine
  (combine

   ;; Background
   (with-fill-color "white" (filled-rect-drawer 0 0 800 600))

   ;; Filled circle
   (with-fill-color "red"
     (filled-circle-drawer 200 200 50))

   ;; Line with dashed style
   (with-pen-color "red"
     (with-line-width 3
       (with-dash '(10.0 5.0) 0.0
         (line-drawer 100 100 700 500))))

   ;; Polygon (filled + border)
   (with-fill-color "lightgreen"
     (with-pen-color "darkgreen"
       (with-line-width 1.5
         (filled-polygon-drawer '((400 . 100) (500 . 150) (450 . 250) (350 . 200))))))

   ;; Text title
   (with-pen-color "black"
     (with-font "sans" 12.0 'normal 'normal
       (text-drawer 400 50 "Primitives Demo")))
   ))

;; Render to PNG
(define (run-example-basic-shapes)
  (render-to-png example-basic-shapes "example-basic-shapes.png" 800 600))

;;; ========================================================================
;;; Style combinators
;;; ========================================================================

(define example-style-transform
  ;; style transformation — showing styled circles and shapes
  (let ((base-circle (with-fill-color "orange"
                       (filled-circle-drawer 0 0 30))))
    (combine
     ;; Background
     (with-fill-color "white" (filled-rect-drawer 0 0 800 600))

     ;; Original at center
     (with-translate 400 300 base-circle)

     ;; Translated: outlined circle (blue pen, no fill)
     (with-translate 200 300
       (with-pen-color "blue"
         (with-line-width 2
           (circle-drawer 0 0 30))))

     ;; Translated: outlined circle (red, thicker)
     (with-translate 600 300
       (with-pen-color "red"
         (with-line-width 4
           (circle-drawer 0 0 30))))

     ;; Manually "scaled" version: larger circle (radius 45 = 30 * 1.5)
     (with-translate 400 150
       (with-fill-color "orange"
         (filled-circle-drawer 0 0 45)))

     ;; Diamond polygon in place of the rotated rectangle
     (with-translate 400 450
       (with-fill-color "purple"
         (with-pen-color "white"
           (with-line-width 2
             (filled-polygon-drawer
               '(( 0 . -28)   ; top
                 (40 .   0)   ; right
                 ( 0 .  28)   ; bottom
                 (-40 .  0))))))) ; left
     )))

(define (run-example-style-transform)
  (render-to-png example-style-transform "example-style-transform.png" 800 600))

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
               (with-fill-color "lightblue"
                 (filled-circle-drawer 25 25 20))
               (with-pen-color "darkblue"
                 (circle-drawer 25 25 20))
               (with-pen-color "navy"
                 (with-font "sans" 15.0 'normal 'normal
                   (text-drawer 25 25 "S"
                                #:halign halign/center
                                #:valign valign/center))))))
    (combine
     (with-fill-color "white" (filled-rect-drawer 0 0 800 600))
     (make-grid 4 6 130 130 cell))))

(define (run-example-grid-of-shapes)
  (render-to-svg example-grid-of-shapes "example-grid-of-shapes.svg" 800 600))

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
  ;; polylines and cubic Bezier curves
  (combine
   ;; Background
   (with-fill-color "white" (filled-rect-drawer 0 0 800 600))

   ;; Sine wave as polyline
   (with-pen-color "blue"
     (with-line-width 2
       (polyline-drawer (generate-sine-wave 50 750 100 50))))

   ;; Cubic Bezier curve via path-drawer
   (with-pen-color "red"
     (with-line-width 2
       (path-drawer
         (list (path:move-to 100.0 500.0)
               (path:curve-to 200.0 550.0 600.0 400.0 700.0 500.0)))))

   ;; Control-point markers
   (with-fill-color "red" (filled-circle-drawer 100 500 5))
   (with-fill-color "red" (filled-circle-drawer 200 550 5))
   (with-fill-color "red" (filled-circle-drawer 600 400 5))
   (with-fill-color "red" (filled-circle-drawer 700 500 5))

   ;; Labels
   (with-pen-color "blue"  (with-font "sans" 12.0 'normal 'normal (text-drawer  50 250 "Sine Wave")))
   (with-pen-color "red"   (with-font "sans" 12.0 'normal 'normal (text-drawer  50 450 "Bezier Curve")))))

(define (run-example-polylines)
  (render-to-png example-polylines "example-polylines.png" 800 600))

;;; ========================================================================
;;; Simple plot-like visualization
;;; ========================================================================

(define (scatter-plot points x-min x-max y-min y-max
                      width height margin)
  "Create a simple scatter plot from data points"
  (let* ((plot-width  (- width  (* 2 margin)))
         (plot-height (- height (* 2 margin)))
         (x-scale (lambda (x)
                    (+ margin (* plot-width
                                 (/ (- x x-min) (- x-max x-min))))))
         (y-scale (lambda (y)
                    (+ margin (* plot-height
                                 (/ (- y y-min) (- y-max y-min)))))))

    (combine
     ;; Background
     (with-fill-color "white" (filled-rect-drawer 0 0 width height))

     ;; Plot area background
     (with-fill-color "gray95" (filled-rect-drawer margin margin plot-width plot-height))

     ;; Axes
     (with-pen-color "black"
       (with-line-width 2
         (combine
           (line-drawer margin margin margin (+ margin plot-height))
           (line-drawer margin margin (+ margin plot-width) margin))))

     ;; Data points
     (apply combine
            (map (lambda (pt)
                   (combine
                     (with-fill-color "steelblue"
                       (filled-circle-drawer (x-scale (car pt)) (y-scale (cdr pt)) 4))
                     (with-pen-color "navy"
                       (circle-drawer (x-scale (car pt)) (y-scale (cdr pt)) 4))))
                 points))

     ;; Axis labels
     (with-pen-color "black"
       (with-font "sans" 12.0 'normal 'normal
         (combine
           (text-drawer (+ margin (/ plot-width 2)) 10 "X Axis"
                        #:halign halign/center)
           (text-drawer 20 (+ margin (/ plot-height 2)) "Y Axis"
                        #:halign halign/center)))))))

(define example-simple-scatter-plot
  ; scatter plot demonstration
  (let ((data '((1 . 2.3) (2 . 4.1) (3 . 3.5) (4 . 5.8)
                (5 . 5.2) (6 . 7.1) (7 . 6.8) (8 . 8.2))))
    (scatter-plot data 0 10 0 10 800 600 50)))

(define (run-example-simple-scatter-plot)
  (render-to-png example-simple-scatter-plot "example-simple-scatter-plot.png" 800 600))

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
                                (with-pen-color "black"
                                  (with-line-width 1
                                    (line-drawer t (- y (/ spike-height 2))
                                                 t (+ y (/ spike-height 2))))))
                              spike-times))))
              (iota trial-count))))

(define example-spike-raster
  ; Neural spike raster
  (let ((random-spikes
         (lambda (trial)
           ;; Generate pseudo-random spike times
           (filter (lambda (t) (< (pseudo-random-integer 100) 30))
                   (iota 100)))))
    (combine
     ;; Background
     (with-fill-color "white" (filled-rect-drawer 0 0 800 600))

     ;; Stimulus period marker
     (with-fill-color "yellow"
       (filled-rect-drawer 200 50 100 500))

     ;; Spike rasters
     (with-translate 50 50
       (spike-raster 20 random-spikes 0 25 20))

     ;; Axis line at bottom (Y-up: small y = near bottom of canvas)
     (with-pen-color "black"
       (with-line-width 2
         (line-drawer 50 25 750 25)))

     ;; Labels
     (with-pen-color "black"  (with-font "sans" 12.0 'normal 'normal (text-drawer 400  10 "Time (ms)" #:halign halign/center)))
     (with-pen-color "black"  (with-font "sans" 12.0 'normal 'normal (text-drawer  20 300 "Trials")))
     (with-pen-color "orange" (with-font "sans" 10.0 'normal 'normal (text-drawer 300 570 "Stimulus"))))))

(define (run-example-spike-raster)
  (render-to-png example-spike-raster "example-spike-raster.png" 800 600))

;;; ========================================================================
;;; Complex composition - Dashboard layout
;;; ========================================================================

(define (panel x y width height title drawer)
  "Create a titled panel containing a drawer"
  (combine
   ;; Panel background
   (with-fill-color "white"
     (with-pen-color "gray"
       (with-line-width 1
         (filled-rect-drawer x y width height))))
   ;; Title bar
   (with-fill-color "lightgray"
     (filled-rect-drawer x (+ y height -25) width 25))
   ;; Title text
   (with-pen-color "black"
     (with-font "sans" 12.0 'normal 'normal
       (text-drawer (+ x 80) (+ y height -10) title)))
   ;; Content with proper translation
   (with-translate x y drawer)))

(define example-multi-panel
  ; Multi-panel dashboard layout
  (combine
   ;; Background
   (with-fill-color "whitesmoke" (filled-rect-drawer 0 0 1200 800))

   ;; Panel 1: Circle
   (panel 20 20 350 350 "Panel 1: Geometry"
          (with-fill-color "coral"
            (with-pen-color "darkred"
              (with-line-width 2
                (combine
                  (filled-circle-drawer 175 175 100)
                  (circle-drawer 175 175 100))))))

   ;; Panel 2: Line chart
   (panel 390 20 350 350 "Panel 2: Time Series"
          (with-pen-color "steelblue"
            (with-line-width 2
              (polyline-drawer (generate-sine-wave 20 330 50 100)))))

   ;; Panel 3: Grid pattern
   (panel 760 20 420 350 "Panel 3: Pattern"
          (make-grid 4 6 70 70
                     (combine
                       (with-fill-color "lightgreen"
                         (filled-circle-drawer 35 35 25))
                       (with-pen-color "darkgreen"
                         (circle-drawer 35 35 25)))))

   ;; Panel 4: Text and labels
   (panel 20 390 560 390 "Panel 4: Annotations"
          (combine
           (with-pen-color "navy"
             (with-font "sans" 25.0 'normal 'normal
               (text-drawer 120 200 "Large text label")))
           (with-pen-color "gray"
             (with-font "sans" 12.0 'normal 'normal
               (text-drawer 80 100 "Smaller annotation")))))))

(define (run-example-multi-panel)
  (render-to-png example-multi-panel "example-multi-panel.png" 1200 800))

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
  (run-example-multi-panel))

(run-all-examples)
