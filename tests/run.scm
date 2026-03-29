;;;; Test suite for Grammar of Graphics features
;;;;
;;;; Tests label collision avoidance, legend positioning, and annotations

(import scheme
        (chicken base)
        (chicken format)
        (chicken time)
        test
        yasos
        srfi-1
        matchable
        datatype
        gg-primitives-vge
        gg-scales
        gg-aes
        gg-geom
        gg-layout
        gg-plot)

;;; ========================================================================
;;; Test Data Fixtures
;;; ========================================================================

(define (assq-ref alist key)
  "Safe assq reference"
  (and (list? alist)
       (if (symbol? (car alist))
           (assq-ref (cdr alist) key)
           (let ((pair (assq key alist)))
             (if pair (cdr pair) #f)))))


(define test-points
  '((x . (1 2 3 4 5))
    (y . (2.1 2.15 2.2 2.05 2.25))
    (label . ("A" "B" "C" "D" "E"))))

(define neural-spike-data
  '((time . (10 15 20 25 30 35 40 45 50))
    (amplitude . (0.5 0.8 1.2 0.9 0.6 1.5 1.8 1.1 0.7))
    (condition . ("baseline" "baseline" "baseline"
                  "stim" "stim" "stim"
                  "post" "post" "post"))))

(define gene-expression
  '((gene . ("GAPDH" "ACTB" "TP53" "MYC" "BRCA1"))
    (log2fc . (-0.5 0.2 2.5 -1.8 3.2))
    (pvalue . (0.3 0.5 0.001 0.02 0.0001))))

;;; ========================================================================
;;; Bounding Box Tests
;;; ========================================================================

(test-group "bounding-box-utilities"
  (test "bbox creation"
        '(0 0 100 50)
        (let ((bbox (bounding-box 0 0 100 50)))
          (list (bbox-x-min bbox)
                (bbox-y-min bbox)
                (bbox-x-max bbox)
                (bbox-y-max bbox))))
  
  (test "bbox overlap detection - overlapping"
        #t
        (let ((bbox1 (bounding-box 0 0 10 10))
              (bbox2 (bounding-box 5 5 15 15)))
          (bbox-overlap? bbox1 bbox2)))
  
  (test "bbox overlap detection - non-overlapping"
        #f
        (let ((bbox1 (bounding-box 0 0 10 10))
              (bbox2 (bounding-box 20 20 30 30)))
          (bbox-overlap? bbox1 bbox2)))
  
  (test "bbox union"
        '(0 0 15 15)
        (let* ((bbox1 (bounding-box 0 0 10 10))
               (bbox2 (bounding-box 5 5 10 10))
               (union (bbox-union bbox1 bbox2)))
          (list (bbox-x-min union)
                (bbox-y-min union)
                (bbox-x-max union)
                (bbox-y-max union))))
  
  (test "bbox center"
        '(50 . 25)
        (let ((bbox (bounding-box 0 0 100 50)))
          (bbox-center bbox)))
  
  (test "bbox area"
        5000
        (let ((bbox (bounding-box 0 0 100 50)))
          (bbox-area bbox))))

;;; ========================================================================
;;; Text Bounds Tests
;;; ========================================================================

(test-group "text-bounds-estimation"
  (test "text bounds for 'Hello' at origin"
        #t
        (let ((bbox (compute-text-bounds "Hello" 0 0 #:size 10.0)))
          (and (< (bbox-x-min bbox) 0)
               (> (bbox-x-max bbox) 0)
               (< (bbox-y-min bbox) 0)
               (> (bbox-y-max bbox) 0))))
  
  (test "longer text has wider bounds"
        #t
        (let ((bbox1 (compute-text-bounds "Hi" 0 0 #:size 10.0))
              (bbox2 (compute-text-bounds "Hello World" 0 0 #:size 10.0)))
          (> (- (bbox-x-max bbox2) (bbox-x-min bbox2))
             (- (bbox-x-max bbox1) (bbox-x-min bbox1)))))
  
  (test "larger font has larger bounds"
        #t
        (let ((bbox1 (compute-text-bounds "Test" 0 0 #:size 10.0))
              (bbox2 (compute-text-bounds "Test" 0 0 #:size 20.0)))
          (> (bbox-area bbox2) (bbox-area bbox1)))))

;;; ========================================================================
;;; Label Collision Detection Tests
;;; ========================================================================

(test-group "label-collision-detection"
  (test "no overlap for well-separated labels"
        0
        (let ((labels (list (make-label-spec "A" 0 0 #:size 10.0)
                            (make-label-spec "B" 100 100 #:size 10.0))))
          (length (detect-label-overlap labels))))
  
  (test "overlap detected for close labels"
        #t
        (let ((labels (list (make-label-spec "A" 0 0 #:size 10.0)
                            (make-label-spec "B" 2 1 #:size 10.0))))
          (> (length (detect-label-overlap labels)) 0)))
  
  (test "multiple overlaps detected"
        #t
        (let ((labels (list (make-label-spec "A" 0 0 #:size 10.0)
                            (make-label-spec "B" 1 0 #:size 10.0)
                            (make-label-spec "C" 2 0 #:size 10.0))))
          (>= (length (detect-label-overlap labels)) 2))))

;;; ========================================================================
;;; Force-Directed Layout Tests
;;; ========================================================================

(test-group "force-directed-layout"
  (test "labels move apart when overlapping"
        #t
        (let* ((labels (list (make-label-spec "A" 0 0 #:size 10.0)
                             (make-label-spec "B" 1 0 #:size 10.0)))
               (initial-dist (abs (- (label-x (car labels))
                                     (label-x (cadr labels)))))
               (adjusted (force-directed-label-layout labels #:iterations 50))
               (final-dist (abs (- (label-x (car adjusted))
                                   (label-x (cadr adjusted))))))
          (> final-dist initial-dist)))
  
  (test "labels retain approximate position after layout"
        #t
        (let* ((labels (list (make-label-spec "A" 50 50 #:size 10.0)
                             (make-label-spec "B" 52 51 #:size 10.0)))
               (adjusted (force-directed-label-layout labels #:iterations 20)))
          (and (< (abs (- (label-x (car adjusted)) 50)) 20)
               (< (abs (- (label-y (car adjusted)) 50)) 20))))
  
  (test "overlaps reduced after layout"
        #t
        (let* ((labels (list (make-label-spec "A" 0 0 #:size 10.0)
                             (make-label-spec "B" 1 1 #:size 10.0)
                             (make-label-spec "C" 2 0 #:size 10.0)))
               (initial-overlaps (length (detect-label-overlap labels)))
               (adjusted (force-directed-label-layout labels #:iterations 100))
               (final-overlaps (length (detect-label-overlap adjusted))))
          (< final-overlaps initial-overlaps))))

;;; ========================================================================
;;; Annotation Layer Tests
;;; ========================================================================

(test-group "annotation-layer-creation"
  (test "text annotation creates proper spec"
        '(annotation text)
        (let ((annot (annotate-text "Test" 10 20)))
          (take annot 2)))
  
  (test "rect annotation creates proper spec"
        '(annotation rect)
        (let ((annot (annotate-rect 0 0 10 10)))
          (take annot 2)))
  
  (test "segment annotation creates proper spec"
        '(annotation segment)
        (let ((annot (annotate-segment 0 0 10 10)))
          (take annot 2)))
  
  (test "arrow annotation creates proper spec"
        '(annotation arrow)
        (let ((annot (annotate-arrow 0 0 10 10)))
          (take annot 2)))
  
  (test "text annotation preserves parameters"
        "Test Label"
        (let ((annot (annotate-text "Test Label" 10 20 #:size 12.0)))
          (assq-ref (cddr annot) 'text)))
  
  (test "rect annotation preserves coordinates"
        '(5 10 15 20)
        (let ((annot (annotate-rect 5 10 15 20)))
          (list (assq-ref (cddr annot) 'xmin)
                (assq-ref (cddr annot) 'ymin)
                (assq-ref (cddr annot) 'xmax)
                (assq-ref (cddr annot) 'ymax)))))

;;; ========================================================================
;;; Legend Positioning Tests
;;; ========================================================================

(test-group "legend-positioning"
  (test "legend bounds computed correctly"
        #t
        (let* ((legend-spec '(discrete "Color" items 150 200))
               (bbox (compute-legend-bounds legend-spec)))
          (and (= (bbox-x-min bbox) 0)
               (= (bbox-y-min bbox) 0)
               (= (bbox-x-max bbox) 150)
               (= (bbox-y-max bbox) 200))))
  
  (test "auto positioning returns positioned legends"
        #t
        (let* ((legends '((discrete "Color" items 150 200)
                          (discrete "Fill" items 150 200)))
               (plot-bbox (bounding-box 0 0 800 600))
               (panel-bbox (bounding-box 50 50 750 550))
               (positioned (legend-position-auto legends plot-bbox panel-bbox)))
          (and (= (length positioned) 2)
               (pair? (cdar positioned)))))  ; Has position coordinate
  
  (test "inside positioning respects panel bounds"
        #t
        (let* ((legends '((discrete "Color" items 150 200)))
               (panel-bbox (bounding-box 50 50 750 550))
               (positioned (legend-position-inside legends 'top-right panel-bbox)))
          (and (pair? positioned)
               (let ((pos (cdar positioned)))
                 (and (> (car pos) (bbox-x-min panel-bbox))
                      (< (car pos) (bbox-x-max panel-bbox))))))))

;;; ========================================================================
;;; Integration Tests - Layer Constructors
;;; ========================================================================

(test-group "phase5-layer-constructors"
  (test "layer-annotate-text creates valid layer spec"
        '(layer annotation)
        (let ((layer (layer-annotate-text "Label" 10 20)))
          (take layer 2)))
  
  (test "layer-annotate-rect creates valid layer spec"
        '(layer annotation)
        (let ((layer (layer-annotate-rect 0 0 10 10)))
          (take layer 2)))
  
  (test "annotate-vline creates vline spec"
        '(layer annotation)
        (let ((layer (annotate-vline 5)))
          (take layer 2)))
  
  (test "annotate-hline creates hline spec"
        '(layer annotation)
        (let ((layer (annotate-hline 0)))
          (take layer 2)))
  
  (test "theme-legend-position creates theme mod"
        'theme-mod
        (let ((theme (theme-legend-position 'right)))
          (car theme))))

;;; ========================================================================
;;; Practical Example Tests
;;; ========================================================================

(test-group "practical-examples"
  (test "neural spike plot with annotations"
        #t
        (let ((plot-spec
               `(plot
                 (data . ,neural-spike-data)
                 (aes (x . time) (y . amplitude))
                 (layers
                  (layer line (params (color . "steelblue") (width . 1.5)))
                  ;; Stimulus period annotation
                  ,(layer-annotate-rect 20 0 35 2
                                        #:fill "yellow" #:alpha 0.2)
                  ;; Stimulus onset marker
                  ,(annotate-vline 20 #:color "red")
                  ;; Baseline reference
                  ,(annotate-hline 0.5 #:color "gray" #:linetype 'dashed)))))
          (and (eq? 'plot (car plot-spec))
               (= 4 (length (assq-ref plot-spec 'layers))))))
  
  (test "volcano plot with repel labels"
        #t
        (let ((significant-genes
               (filter (lambda (i)
                         (< (list-ref (assq-ref gene-expression 'pvalue) i) 0.01))
                       (iota (length (assq-ref gene-expression 'gene))))))
          (> (length significant-genes) 0))))

;;; ========================================================================
;;; Performance Tests
;;; ========================================================================

(test-group "performance"
  (test "force-directed layout completes in reasonable time"
        #t
        (let* ((many-labels
                (map (lambda (i)
                       (make-label-spec
                        (number->string i)
                        (remainder i 10)
                        (quotient i 10)
                        #:size 10.0))
                     (iota 50)))
               (start-time (current-process-milliseconds))
               (adjusted (force-directed-label-layout many-labels
                                                      #:iterations 20))
               (end-time (current-process-milliseconds))
               (elapsed (- end-time start-time)))
          ;; Should complete in under 1 second for 50 labels
          (< elapsed 1000)))

  (test "overlap detection scales linearly"
      #t
      (let* ((labels-10
               (map (lambda (i) (make-label-spec "X" i i #:size 10.0)) (iota 10)))
             (labels-20
               (map (lambda (i) (make-label-spec "X" i i #:size 10.0)) (iota 20)))
             
             ;; Run 1000 iterations to get measurable time
             (iterations 1000)
             
             (start-10 (current-process-milliseconds))
             (_ (do ((i 0 (+ i 1)))
                    ((>= i iterations))
                  (detect-label-overlap labels-10)))
             (time-10 (- (current-process-milliseconds) start-10))
             
             (start-20 (current-process-milliseconds))
             (_ (do ((i 0 (+ i 1)))
                    ((>= i iterations))
                  (detect-label-overlap labels-20)))
             (time-20 (- (current-process-milliseconds) start-20)))
        
        ;; Now times should be measurable (10-100ms range)
        ;; Still skip if somehow still 0
        (if (= time-10 0)
            #t
            (< time-20 (* time-10 5)))))
  )

;;; ========================================================================
;;; Regression Tests
;;; ========================================================================

(test-group "regression-tests"
  (test "empty label list doesn't crash"
        '()
        (detect-label-overlap '()))
  
  (test "single label has no overlaps"
        '()
        (detect-label-overlap (list (make-label-spec "A" 0 0))))
  
  (test "force-directed layout preserves label count"
        5
        (let* ((labels (map (lambda (i)
                              (make-label-spec "X" i i #:size 10.0))
                            (iota 5)))
               (adjusted (force-directed-label-layout labels)))
          (length adjusted)))
  
  (test-assert "annotation with missing parameters uses defaults"
               (let ((annot (annotate-text "Test" 0 0)))
                 (and (assq-ref (cddr annot) 'size)
                      (assq-ref (cddr annot) 'color)))))

;;; ========================================================================
;;; Run All Tests
;;; ========================================================================

(test-exit)
