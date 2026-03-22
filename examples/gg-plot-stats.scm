;;;; Examples demonstrating statistical graphics

(import scheme
        (chicken base)
        (chicken format)
        (chicken string)
        statistics
        gg-plot
        gg-scales
        gg-aes
        gg-geom
        gg-backend-cairo)

;; Test data sets
(define summary-data 
  '((treatment . (a b c))
    (mean . (10.5 12.3 15.7))
    (lower . (9.2 11.1 14.5))
    (upper . (11.8 13.5 16.9))))

(define time-series-data
  '((time . (1.0 2.0 3.0 4.0 5.0))
    (value . (10.5 12.3 15.7 14.2 16.8))
    (lower . (9.2 11.1 14.5 13.0 15.5))
    (upper . (11.8 13.5 16.9 15.4 18.1))))

(include "examples/iris-dataset.scm")

; Categorical error bars with pre-computed bounds

(define p1
  (ggplot summary-data (aes #:x 'treatment #:y 'mean 
                            #:ymin 'lower #:ymax 'upper)
          (layer-errorbar #:cap-width 0.3 #:color "darkred")
          (layer-point #:size 6)
          (labs #:title "Categorical Data with Error Bars")
          ))

(define p2
  (ggplot time-series-data (aes #:x 'time #:y 'value 
                                #:ymin 'lower #:ymax 'upper)
          (layer-line #:color "blue")
          (layer-errorbar #:cap-width 0.1 #:color "darkblue")
          (layer-point #:size 5 #:color "blue")
          (labs #:title "Time Series with Error Bars")
          ))

;; Compute error bars from raw data (mean +/- SE)
(define p2.1
  (ggplot iris (aes #:x 'species #:y 'sepal_length)
          (layer-pointrange #:stat 'summary #:color "steelblue")
          (labs #:title "Mean Sepal Length +/- SE by Species")))


;; Custom error bar functions (mean +/- 2*SE for 95% CI approx)
(define p3
  (ggplot iris (aes #:x 'species #:y 'petal_length)
          (layer-errorbar #:stat 'summary
                          #:fun-ymin (lambda (vals) 
                                       (- (mean vals) 
                                          (* 2 (standard-error-of-the-mean vals))))
                          #:fun-ymax (lambda (vals)
                                       (+ (mean vals)
                                          (* 2 (standard-error-of-the-mean vals))))
                          #:color "purple")
          (layer-point #:stat 'summary #:size 5 #:color "purple")
          (labs #:title "Mean Petal Length +/- 2SE (~= 95% CI)")))


;; Linerange (simpler than errorbar, no caps)
(define p4
  (ggplot iris (aes #:x 'species #:y 'sepal_width)
          (layer-linerange #:stat 'summary #:width 3 #:color "darkgreen")
          (layer-point #:stat 'summary #:size 6 #:color "darkgreen")
          (labs #:title "Mean Sepal Width with SE (linerange)")))


;; Crossbar (horizontal bar with center line)
(define p5
  (ggplot iris (aes #:x 'species #:y 'petal_width)
          (layer-crossbar #:stat 'summary #:fill "lightblue" #:color "navy")
          (labs #:title "Mean Petal Width with SE (crossbar)")))


;; Combined column chart with error bars
(define p6
  (ggplot iris (aes #:x 'species #:y 'sepal_length)
          (layer-col #:fill "steelblue")
          (layer-errorbar #:stat 'summary #:width 0.2 #:color "black")
          (labs #:title "Mean Sepal Length by Species")))


;; Custom summary function (median +/- IQR/2)
(define p7
  (ggplot iris (aes #:x 'species #:y 'petal_length)
          (layer-pointrange #:stat 'summary
                            #:fun-y median
                            #:fun-ymin (lambda (vals) 
                                         (- (median vals) (/ (iqr vals) 2)))
                            #:fun-ymax (lambda (vals)
                                         (+ (median vals) (/ (iqr vals) 2)))
                            #:color "coral")
          (labs #:title "Median Petal Length +/- IQR/2")))


;; Horizontal error bars (using xmin/xmax)

(define p8
  (ggplot time-series-data (aes #:y 'value #:xmin 'lower #:xmax 'upper)
          (layer-linerange #:color "purple" #:width 2)
          (labs #:title "Horizontal Time Ranges")))

;;;; ========================================================================
;;;; Usage Examples and Tests
;;;; ========================================================================

(for-each
 (lambda (plot name)
   (ggsave plot (string-append name ".png") #:width 900 #:height 600))
 (list p1 p2 p2.1 p3 p4 p5 p6 p7 p8)
 (list "p1-errorbar"
       "p2-errorbar"
       "p2.1-errorbar"
       "p3-custom-errorbar"
       "p4-linerange"
       "p5-crossbar"
       "p6-column-chart"
       "p7-pointrange"
       "p8-horz-errorbar"))


#|

;; Example: Test stat-bin transformation
(let* ((data '((x . (1 2 3 4 5 6 7 8 9 10))))
       (aes (aes #:x 'x))
       (params '((bins . 5) (method . equal-width))))
  (stat-bin data aes params))
;; => ((x . (1.9 3.7 5.5 7.3 9.1))
;;     (width . (1.8 1.8 1.8 1.8 1.8))
;;     (y . (2 2 2 2 2)))


;; Example: Test stat-density transformation
(let* ((data '((values . (1 2 3 4 5))))
       (aes (aes #:x 'values))
       (params '((kernel . gaussian) (n . 10))))
  (stat-density data aes params))
;; => ((x . (x1 x2 ... x10))
;;     (y . (density1 density2 ... density10)))


;; Example: Test stat-boxplot transformation
(let* ((data '((species . (a a a b b b))
               (value . (1 2 3 4 5 100))))
       (aes (aes #:x 'species #:y 'value))
       (params '((coef . 1.5))))
  (stat-boxplot data aes params))
;; => ((x . (a b))
;;     (ymin . (1.0 4.0))
;;     (lower . (1.5 4.5))
;;     (middle . (2.0 5.0))
;;     (upper . (2.5 5.5))
;;     (ymax . (3.0 6.0))
;;     (outliers . (() (100))))


;; Example: Test stat-summary transformation
(let* ((data '((group . (a a a b b b))
               (response . (10 12 14 20 22 24))))
       (aes (aes #:x 'group #:y 'response))
       (params '()))
  (stat-summary data aes params))
;; => ((x . (a b))
;;     (y . (12.0 22.0))  ; means
;;     (ymin . (10.845... 20.845...))  ; mean - SE
;;     (ymax . (13.154... 23.154...)))  ; mean + SE

|#
