;;;; gg-tier3-examples.scm
;;;; Examples demonstrating Geometries and Aesthetic Mapping

(import scheme
        (chicken base)
        (chicken random)
        gg-primitives
        gg-scales
        gg-guides
        gg-aes
        gg-data
        gg-geom
        srfi-1)

;;; ========================================================================
;;; Basic scatter plot with geom-point
;;; ========================================================================

(define example-geom-point
  ;; Scatter plot using geom-point with automatic scale training
  (let* (;; Data
         (data '((x . (1 2 3 4 5 6 7 8 9 10))
                 (y . (2.3 4.1 3.5 5.8 5.2 7.1 6.8 8.2 7.5 9.1))))
         (aes-map (aes x: 'x y: 'y))
         )
    (let-values (((points scales)
                  (geom-with-ranges geom-point data aes-map
                                   '(80 . 720)
                                   '(80 . 520)
                                   #:size 6)))

       (let* ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales)))
              (x-axis (make-axis-bottom x-scale #:label "X Variable"))
              (y-axis (make-axis-left y-scale #:label "Y Variable")))
         
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         points
         (text 400 560 "Scatter Plot with geom-point"
               #:color "black" #:size 15.0))))
    ))

(define (run-example-geom-point)
  (render example-geom-point
          (make-png-plotter "example-geom-point.png" 800 600)))

;;; ========================================================================
;;; Line plot with geom-line
;;; ========================================================================

(define example-geom-line
  ;; Line plot using geom-line
  (let* (;; Time series data
         (data '((time . (0 1 2 3 4 5 6 7 8 9 10))
                 (value . (0.5 1.2 0.8 1.5 1.8 1.3 2.1 1.9 2.4 2.2 2.8))))
         
         (aes-map (aes #:x 'time #:y 'value))
         )
         
    (let-values (((lines scales)
                  (geom-with-ranges geom-line data aes-map
                                   '(80 . 720)
                                   '(80 . 520)
                                    #:width 2)))

       (let* ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales)))
              (x-axis (make-axis-bottom x-scale #:label "X Variable"))
              (y-axis (make-axis-left y-scale #:label "Y Variable")))
        
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         lines
         (text 400 560 "Line Plot with geom-line"
               #:color "black" #:size 15.0))))
    ))
  

(define (run-example-geom-line)
  (render example-geom-line
          (make-png-plotter "example-geom-line.png" 800 600)))

;;; ========================================================================
;;; Bar chart with geom-bar
;;; ========================================================================

(define example-geom-bar
  ;; Bar chart using geom-bar
  (let* (;; Categorical data
         (data '((category . ("A" "B" "C" "D" "E"))
                 (count . (23 45 12 67 34))))
         
         (aes-map (aes #:x 'category #:y 'count))
         )
    (let-values (((bars scales)
                  (geom-with-ranges geom-bar data aes-map
                                    '(80 . 720)
                                    '(80 . 520)
                                    #:fill "coral")))
      
      (let* ((x-scale (cdr (assoc 'x scales)))
             (y-scale (cdr (assoc 'y scales)))
             (x-axis (make-axis-bottom x-scale #:label "Category"))
             (y-axis (make-axis-left y-scale #:label "Count")))
        
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         bars  ; No translation - already in absolute coordinates
         (text 400 560 "Bar Chart with geom-bar"
               #:color "black" #:size 15.0))))))
      
(define (run-example-geom-bar)
  (render example-geom-bar
          (make-png-plotter "example-geom-bar.png" 800 600)))

;;; ========================================================================
;;; Grouped line plot
;;; ========================================================================

(define example-line-plot
  ;; Multiple lines grouped by category
  (let* (;; Data with groups
         (data '((time . (0 1 2 3 4 5 0 1 2 3 4 5))
                 (value . (1.0 1.5 1.3 1.8 2.0 1.9 0.5 0.8 1.2 1.0 1.5 1.7))
                 (group . ("A" "A" "A" "A" "A" "A" "B" "B" "B" "B" "B" "B"))))
         
         (aes-map (aes #:x 'time #:y 'value #:group 'group))
         )
    (let-values (((lines scales)
                  (geom-with-ranges geom-line data aes-map
                                    '(80 . 720)
                                    '(80 . 520)
                                    #:width 2)))

       (let* ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales)))
              (x-axis (make-axis-bottom x-scale #:label "X Variable"))
              (y-axis (make-axis-left y-scale #:label "Y Variable")))
    
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         lines
         (text 400 560 "Grouped Lines"
               #:color "black" #:size 15.0))))
    ))

(define (run-example-line-plot)
  (render example-line-plot
          (make-png-plotter "example-line-plot.png" 800 600)))

;;; ========================================================================
;;; Area plot with geom-area
;;; ========================================================================

(define example-geom-area
  ;; Area plot showing filled region
  (let* (;; Data
         (data '((x . (0 1 2 3 4 5 6 7 8))
                 (y . (1.0 1.5 1.2 1.8 2.3 2.0 2.5 2.2 2.8))))
         
         (aes-map (aes #:x 'x #:y 'y))
         )
    (let*-values (((area scales) (geom-with-ranges geom-area data aes-map
                                                   '(80 . 720)
                                                   '(80 . 520)
                                                   #:fill "steelblue" #:alpha 0.5))
                  ;; Also create line on top
                  ((line-drawer _) (geom-line data aes-map
                                              #:scales scales
                                              #:width 2)))

       (let* ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales)))
              (x-axis (make-axis-bottom x-scale #:label "X Variable"))
              (y-axis (make-axis-left y-scale #:label "Y Variable")))
    
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         area
         line-drawer
         (text 400 560 "Area Plot with geom-area"
               #:color "black" #:size 15.0))))
    ))

(define (run-example-geom-area)
  (render example-geom-area
          (make-png-plotter "example-geom-area.png" 800 600)))

;;; ========================================================================
;;; Text labels with geom-text
;;; ========================================================================

(define example-geom-text
  ;; Scatter plot with labels
  (let* (;; Data with labels
         (data '((x . (1 2 3 4 5))
                 (y . (2.0 3.5 2.8 4.2 3.9))
                 (label . ("Point A" "Point B" "Point C" "Point D" "Point E"))))
         
         (aes-map (aes #:x 'x #:y 'y))
         (label-aes (aes #:x 'x #:y 'y #:label 'label))
         )
    
    (let*-values (((points scales) (geom-with-ranges geom-point data aes-map
                                                     '(80 . 720)
                                                     '(80 . 520)
                                                     #:size 5))
                  ((labels _)      (geom-text data label-aes
                                              #:scales scales
                                              #:size 8.0)))
       (let* ((x-scale (cdr (assoc 'x scales)))
              (y-scale (cdr (assoc 'y scales)))
              (x-axis (make-axis-bottom x-scale #:label "X Variable"))
              (y-axis (make-axis-left y-scale #:label "Y Variable")))

    
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         points
         (with-translate 0 15 labels)
         (text 400 560 "Points with Labels"
               #:color "black" #:size 15.0))))
    ))

(define (run-example-geom-text)
  (render example-geom-text
          (make-png-plotter "example-geom-text.png" 800 600)))

;;; ========================================================================
;;; Spike raster with geom-eventplot (neuroscience)
;;; ========================================================================

(define (extract-all-spikes spike-data)
  "Extract all spike times across all trials, flattened into one list"
  
  ;; Get the spikes column (list of lists)
  (let ((spike-lists (data-column spike-data 'spikes)))
    ;; Flatten the nested lists
    (apply append spike-lists)))

(define example-geom-eventplot
  ;; Spike raster plot for neural data
  (let* (;; Spike train data: each trial has a list of spike times
         (data '((trial . (0 1 2 3 4 5 6 7 8 9))
                 (spikes . ((12 45 78 89)
                            (15 42 51 77 88 95)
                            (10 38 55 72)
                            (18 44 66 91)
                            (11 39 58 74 92)
                            (14 41 53 79 90)
                            (16 43 67 85)
                            (13 40 57 73 89 94)
                            (17 46 69 87)
                            (19 47 71 93)))))

         (aes-map (aes #:x 'spikes #:y 'trial))
         
         ;; Flatten spikes for scale training
         (all-spikes (extract-all-spikes data))
         
         ; Create and train scales manually
         (x-scale (scale-with-range
                   (scale-with-trained (make-scale-linear) all-spikes)
                   '(80 . 720)))
         
         (y-scale (scale-with-range
                   (scale-with-trained (make-scale-linear) (data-column data 'trial))
                   '(80 . 520)))
         
         ;; Pre-configured scales
         (scales (list (cons 'x x-scale) (cons 'y y-scale)))
         )

    (let-values (((raster _) (geom-eventplot data aes-map
                                             #:scales scales
                                             #:line-length 2.8 
                                             #:color "black" 
                                             #:width 2)))
      
      (let* (
             ;; Stimulus period marker
             (stim-start 30)
             (stim-end 60)
             (stim-rect (rectangle (scale-map x-scale stim-start)
                                   80
                                   (- (scale-map x-scale stim-end)
                                      (scale-map x-scale stim-start))
                                   440
                                   #:fill-color "yellow"
                                   #:edge-color "none"))
         
             (x-axis (make-axis-bottom x-scale #:label "Time (ms)"))
             (y-axis (make-axis-left y-scale #:label "Trial"))
             )
    
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "white")
         stim-rect
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         raster
         (text 400 560 "Neural Spike Raster with geom-eventplot"
               #:color "black" #:size 15.0)
         (text 300 540 "Stimulus Period" #:color "orange" #:size 12.0))))
    ))

(define (run-example-geom-eventplot)
  (render example-geom-eventplot
          (make-png-plotter "example-geom-eventplot.png" 800 600)))

;;; ========================================================================
;;; Combined geometries - points + line + area
;;; ========================================================================

(define example-combine-geom
  ;; Layered plot combining multiple geometries
  (let* (;; Data
         (data '((x . (0 1 2 3 4 5 6 7 8 9 10))
                 (y . (1.0 1.8 1.5 2.2 2.5 2.3 2.9 2.7 3.2 3.0 3.5))))
         
         (aes-map (aes #:x 'x #:y 'y))
         )
    
    ;; Create multiple geometry layers
    (let*-values (((area-drawer scales) (geom-with-ranges geom-area data aes-map
                                                          '(80 . 720)
                                                          '(80 . 520)
                                                          #:fill "lightblue"))
                  ((line-drawer _) (geom-line data aes-map #:scales scales #:width 2))
                  ((points _)      (geom-point data aes-map #:scales scales #:size 5)))

      (let* ((x-scale (cdr (assoc 'x scales)))
             (y-scale (cdr (assoc 'y scales)))
             (x-axis (make-axis-bottom x-scale #:label "X Variable"))
             (y-axis (make-axis-left y-scale #:label "Y Variable")))
        
        (combine
         (rectangle 0 0 800 600 #:fill-color "white")
         (rectangle 80 80 640 440 #:fill-color "gray95")
         (with-translate 0 80 (axis-drawer x-axis))
         (with-translate 80 0 (axis-drawer y-axis))
         area-drawer
         line-drawer
         points
         (text 400 560 "Layered Geometries: Area + Line + Points"
               #:color "black" #:size 15.0))))
    ))

(define (run-example-combine-geom)
  (render example-combine-geom
          (make-png-plotter "example-combine-geom.png" 800 600)))

;;; ========================================================================
;;; Run all examples
;;; ========================================================================

(define (run-all-geom-examples)
  (run-example-geom-point)
  (run-example-geom-line)
  (run-example-geom-bar)
  (run-example-line-plot)
  (run-example-geom-area)
  (run-example-geom-text)
  (run-example-geom-eventplot)
  (run-example-combine-geom)
  )
(run-all-geom-examples)

