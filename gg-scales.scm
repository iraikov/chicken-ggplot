;;;; gg-scales.scm
;;;; Grammar of Graphics - Scales
;;;;
;;;; Provides data-to-visual mappings with automatic domain training.

(module gg-scales
  (;; Scale protocol
   scale?
   scale-train!
   scale-map
   scale-inverse
   scale-breaks
   scale-labels
   scale-domain
   scale-range
   scale-bandwidth
   scale-transform
   scale-set-domain!
   scale-set-range!
   scale-type
   
   ;; Functional scale transformations
   scale-with-domain
   scale-with-range
   scale-with-trained
   scale-with-transform
   scale-with-range
   scale-with-breaks
   
   ;; Helper for break computation
   compute-pretty-breaks
   nice-step-size
   nice-breaks-heckbert

   ;; Continuous scales
   make-scale-linear
   make-scale-log
   make-scale-sqrt
   make-scale-power
   
   ;; Discrete scales
   make-scale-ordinal
   make-scale-band
   
   ;; Color scales
   make-scale-color-gradient
   make-scale-color-diverging
   make-scale-color-ordinal
   make-scale-color-manual
   
   ;; Utilities
   nice-breaks
   extend-domain)

  (import scheme
          (chicken base)
          (chicken format)
          yasos
          srfi-1
          srfi-69)  ; Hash tables for ordinal scales

  ;;; ========================================================================
  ;;; Scale Protocol (YASOS)
  ;;; ========================================================================
  ;;;
  ;;; A scale is an object that maps between data domain and visual range.
  ;;; All scales support these operations:
  ;;;   - (scale-train! scale values) - extend domain to include values
  ;;;   - (scale-map scale value) - map from domain to range
  ;;;   - (scale-inverse scale value) - map from range to domain
  ;;;   - (scale-breaks scale [n]) - compute tick positions
  ;;;   - (scale-labels scale breaks) - generate labels for breaks
  ;;;   - (scale-domain scale) - get current domain
  ;;;   - (scale-range scale) - get current range
  ;;;   - (scale-set-domain! scale domain) - set domain explicitly
  ;;;   - (scale-set-range! scale range) - set range explicitly

  (define-operation (scale? obj))
  (define-operation (scale-train! scale values))
  (define-operation (scale-map scale value))
  (define-operation (scale-type scale))
  (define-operation (scale-inverse scale value))
  (define-operation (scale-breaks scale . args))
  (define-operation (scale-labels scale breaks))
  (define-operation (scale-domain scale))
  (define-operation (scale-range scale))
  (define-operation (scale-transform scale))
  (define-operation (scale-set-domain! scale domain))
  (define-operation (scale-set-range! scale range))
  (define-operation (scale-bandwidth scale))
  (define-operation (scale-with-domain scale domain))
  (define-operation (scale-with-range scale range))
  (define-operation (scale-with-trained scale values))
  (define-operation (scale-base scale))
  (define-operation (scale-transform-type scale))
  (define-operation (scale-color-for scale value))
  (define-operation (scale-set-color! scale value color))
  (define-operation (scale-breaks-override scale))
  (define-operation (scale-set-breaks-override! scale breaks))


  ;;; ========================================================================
  ;;; Relative Range Operations
  ;;; ========================================================================
  (define (scale-expand-range scale factor)
    "Expand range by a factor (e.g., 1.1 for 10% expansion)"
    (let* ((range (scale-range scale))
           (r-min (car range))
           (r-max (cdr range))
           (center (/ (+ r-min r-max) 2))
           (half-width (* (- r-max r-min) 0.5 factor)))
      (scale-with-range scale 
                        (cons (- center half-width)
                              (+ center half-width)))))

  (define (scale-pad-range scale padding)
    "Add absolute padding to both ends of range"
    (let* ((range (scale-range scale))
           (r-min (car range))
           (r-max (cdr range)))
      (scale-with-range scale
                        (cons (+ r-min padding)
                              (- r-max padding)))))
  
  (define (scale-shift-range scale dx)
    "Shift entire range by dx"
    (let* ((range (scale-range scale))
           (r-min (car range))
           (r-max (cdr range)))
      (scale-with-range scale
                        (cons (+ r-min dx)
                              (+ r-max dx)))))

  (define (scale-set-range-relative scale min-pct max-pct canvas-size)
    "Set range as percentages of canvas size (0.0 to 1.0)"
    (scale-with-range scale
                      (cons (* canvas-size min-pct)
                            (* canvas-size max-pct))))
  
  ;;; ========================================================================
  ;;; Continuous Scale Base
  ;;; ========================================================================
  ;;;
  ;;; Shared implementation for continuous scales (linear, log, etc.)

  (define (make-continuous-scale type transform inverse-transform)
    "Create a continuous scale with given transform functions"
    (let ((domain #f)    ; (min . max) or #f if not trained
          (range '(0 . 1))) ; Default visual range
      
      (object
       ;; Type predicate
       ((scale? self) #t)

       ((scale-type self) type)
       
       ;; Domain/range accessors
       ((scale-domain self) domain)
       ((scale-range self) range)
       ((scale-set-domain! self new-domain) 
        (set! domain new-domain))
       ((scale-set-range! self new-range) 
        (set! range new-range))
       
       ;; Training: extend domain to include new values
       ((scale-train! self values)
        (when (not (null? values))
          (let ((min-val (apply min values))
                (max-val (apply max values)))
            (if domain
                (set! domain 
                      (cons (min (car domain) min-val)
                            (max (cdr domain) max-val)))
                (set! domain (cons min-val max-val))))))
       
       ;; Mapping: domain -> range
       ((scale-map self value)
        (if (not domain)
            (error "Scale not trained - no domain set")
            (let* ((d-min (car domain))
                   (d-max (cdr domain))
                   (r-min (car range))
                   (r-max (cdr range))
                   ;; Transform to [0,1]
                   (t-val (transform value))
                   (t-min (transform d-min))
                   (t-max (transform d-max))
                   ;; Normalize to [0,1]
                   (normalized (if (= t-min t-max)
                                   0.5
                                   (/ (- t-val t-min) (- t-max t-min)))))
              ;; Map to range
              (+ r-min (* normalized (- r-max r-min))))))
       
       ;; Inverse mapping: range -> domain
       ((scale-inverse self value)
        (if (not domain)
            (error "Scale not trained - no domain set")
            (let* ((d-min (car domain))
                   (d-max (cdr domain))
                   (r-min (car range))
                   (r-max (cdr range))
                   ;; Normalize from range to [0,1]
                   (normalized (if (= r-min r-max)
                                   0.5
                                   (/ (- value r-min) (- r-max r-min))))
                   ;; Transform domain endpoints
                   (t-min (transform d-min))
                   (t-max (transform d-max))
                   ;; Interpolate in transformed space
                   (t-val (+ t-min (* normalized (- t-max t-min)))))
              ;; Inverse transform back to domain
              (inverse-transform t-val))))
       
       ;; Breaks: compute nice tick positions.
       ;; Optional second arg is the method symbol: 'r-pretty (default) or 'heckbert.
       ((scale-breaks self . args)
        (let ((n      (if (pair? args) (car args) 5))
              (method (if (and (pair? args) (pair? (cdr args))) (cadr args) 'r-pretty)))
          (if (not domain)
              '()
              (case method
                ((heckbert) (nice-breaks-heckbert (car domain) (cdr domain) n))
                (else        (nice-breaks (car domain) (cdr domain) n))))))
       
       ;; Labels: format break values
       ((scale-labels self breaks)
        (map (lambda (b) (format "~a" b)) breaks))

       ;; Transform
       ((scale-transform self)
        transform)
       
       ;; Functional updates (return new scale instances)
       ((scale-with-domain self new-domain)
        (let ((new-scale (make-continuous-scale (scale-type self) transform inverse-transform)))
          (scale-set-domain! new-scale new-domain)
          (scale-set-range! new-scale range)
          new-scale))
       
       ((scale-with-range self new-range)
        (let ((new-scale (make-continuous-scale (scale-type self) transform inverse-transform)))
          (when domain (scale-set-domain! new-scale domain))
          (scale-set-range! new-scale new-range)
          new-scale))

       ((scale-with-trained self values)
        (let ((new-scale (make-continuous-scale (scale-type self) transform inverse-transform)))
          (when domain (scale-set-domain! new-scale domain))
          (scale-set-range! new-scale range)
          (scale-train! new-scale values)
          new-scale))
     
       )))

  
  
  ;;; ========================================================================
  ;;; Continuous Scale Constructors
  ;;; ========================================================================

  (define (make-scale-linear #!key domain range)
    "Linear scale: identity transform"
    (let ((scale (make-continuous-scale
                  'linear
                  (lambda (x) x)           ; transform
                  (lambda (x) x))))        ; inverse
      (when domain (scale-set-domain! scale domain))
      (when range (scale-set-range! scale range))
      scale))

  (define (make-scale-log #!key domain range (base 10))
    "Logarithmic scale"
    (let ((scale (make-continuous-scale
                  'log
                  (lambda (x) (log x base))
                  (lambda (x) (expt base x)))))
      (when domain (scale-set-domain! scale domain))
      (when range (scale-set-range! scale range))
      scale))

  (define (make-scale-sqrt #!key domain range)
    "Square root scale"
    (let ((scale (make-continuous-scale
                  'sqrt
                  sqrt
                  (lambda (x) (* x x)))))
      (when domain (scale-set-domain! scale domain))
      (when range (scale-set-range! scale range))
      scale))

  (define (make-scale-power #!key domain range (exponent 2))
    "Power scale with configurable exponent"
    (let ((scale (make-continuous-scale
                  'power
                  (lambda (x) (expt x exponent))
                  (lambda (x) (expt x (/ 1 exponent))))))
      (when domain (scale-set-domain! scale domain))
      (when range (scale-set-range! scale range))
      scale))

  ;;; ========================================================================
  ;;; Discrete Scales
  ;;; ========================================================================

  (define (make-scale-ordinal #!key domain range)
    "Ordinal scale: maps discrete values to discrete outputs"
    (let ((domain-list (or domain '()))
          (range-list (or range '()))
          (value-map (make-hash-table)))
      
      ;; Initialize mapping if both domain and range provided
      (when (and domain range)
        (for-each (lambda (d r)
                    (hash-table-set! value-map d r))
                  domain-list range-list))
      
      (object
       ((scale? self) #t)

       ((scale-type self) 'ordinal)
       
       ((scale-domain self) domain-list)
       ((scale-range self) range-list)
       
       ((scale-set-domain! self new-domain)
        (set! domain-list new-domain)
        ;; Rebuild mapping
        (set! value-map (make-hash-table))
        (for-each (lambda (d r)
                    (hash-table-set! value-map d r))
                  domain-list range-list))
       
       ((scale-set-range! self new-range)
        (set! range-list new-range)
        ;; Rebuild mapping
        (set! value-map (make-hash-table))
        (for-each (lambda (d r)
                    (hash-table-set! value-map d r))
                  domain-list range-list))
       
       ((scale-train! self values)
        ;; Add new domain values not yet seen
        (for-each (lambda (v)
                    (when (not (member v domain-list))
                      (set! domain-list (append domain-list (list v)))
                      ;; Assign next range value (cycling if needed)
                      (let ((idx (- (length domain-list) 1)))
                        (if (null? range-list)
                            (hash-table-set! value-map v idx)
                            (hash-table-set! value-map v 
                                           (list-ref range-list 
                                                    (modulo idx (length range-list))))))))
                  values))
       
       ((scale-map self value)
        (hash-table-ref/default value-map value 
                                (if (null? range-list) 0 (car range-list))))
       
       ((scale-inverse self value)
        ;; Find domain value for range value
        (let ((result #f))
          (hash-table-walk value-map
                          (lambda (k v)
                            (when (equal? v value)
                              (set! result k))))
          result))
       
       ((scale-breaks self . args)
        domain-list)
       
       ((scale-labels self breaks)
        (map (lambda (b) (format "~a" b)) breaks))

       ((scale-with-domain self new-domain)
        (make-scale-ordinal #:domain new-domain #:range range-list))

       ((scale-with-range self new-range)
        (make-scale-ordinal #:domain domain-list #:range new-range))
       
       ((scale-with-trained self values)
        (let ((new-scale (make-scale-ordinal #:domain domain-list #:range range-list)))
          (scale-train! new-scale values)
          new-scale))
       
       )))

  (define (make-scale-band #!key domain range (padding 0.1))
    "Band scale: maps discrete values to continuous bands with spacing"
    (let ((domain-list (or domain '()))
          (range-pair (or range '(0 . 1)))
          (padding-val padding))
      
      (object
       ((scale? self) #t)

       ((scale-type self) 'band)
       
       ((scale-domain self) domain-list)
       ((scale-range self) range-pair)
       ((scale-set-domain! self new-domain) (set! domain-list new-domain))
       ((scale-set-range! self new-range) (set! range-pair new-range))
       
       ((scale-train! self values)
        (for-each (lambda (v)
                    (when (not (member v domain-list))
                      (set! domain-list (append domain-list (list v)))))
                  values))
       
       ((scale-map self value)
        ;; Map value to center of its band.
        ;; Category values are looked up by equality; numeric values are
        ;; treated as 0-based fractional band indices so that annotation
        ;; coordinates like -0.5 (just left of band 0) and 3.5 (just right
        ;; of band 3) work as expected.
        (let* ((n (length domain-list))
               (r-min (car range-pair))
               (r-max (cdr range-pair))
               (total-range (- r-max r-min))
               (step (/ total-range n)))
          (if (number? value)
              ;; Fractional 0-based index → interpolate between bands
              (+ r-min (* value step) (/ step 2))
              ;; Category string/symbol → look up position by domain index
              (let ((idx (list-index (lambda (x) (equal? x value)) domain-list)))
                (if idx
                    (+ r-min (* idx step) (/ step 2))
                    (car range-pair))))))
       
       ((scale-inverse self value)
        ;; Find which band the value falls into
        (let* ((r-min (car range-pair))
               (r-max (cdr range-pair))
               (n (length domain-list))
               (step (/ (- r-max r-min) n))
               (idx (inexact->exact (floor (/ (- value r-min) step)))))
          (if (and (>= idx 0) (< idx n))
              (list-ref domain-list idx)
              #f)))
       
       ((scale-breaks self . args)
        domain-list)
       
       ((scale-labels self breaks)
        (map (lambda (b) (format "~a" b)) breaks))
       
       ;; Additional method for band width
       ((scale-bandwidth self)
        (let* ((r-min (car range-pair))
               (r-max (cdr range-pair))
               (n (length domain-list))
               (step (/ (- r-max r-min) n)))
          (* step (- 1 padding-val))))

       ((scale-with-domain self new-domain)
        (make-scale-band #:domain new-domain 
                         #:range range-pair 
                         #:padding padding-val))
       
       ((scale-with-range self new-range)
        (make-scale-band #:domain domain-list 
                         #:range new-range 
                         #:padding padding-val))
       
       ((scale-with-trained self values)
        (let ((new-scale (make-scale-band #:domain domain-list 
                                          #:range range-pair 
                                          #:padding padding-val)))
          (scale-train! new-scale values)
          new-scale))
       )))

  
  (define (scale-with-transform scale transform-type)
    "Return a new scale with transformation applied.
   
     Since YASOS scales are procedures, we create a wrapper scale
     that applies the transformation during mapping."
  
    (let ((transform-fn (case transform-type
                          ((log log10)
                           (lambda (x) (if (> x 0) (log x 10) -inf.0)))
                          ((log2)
                           (lambda (x) (if (> x 0) (log x 2) -inf.0)))
                          ((ln)
                           (lambda (x) (if (> x 0) (log x) -inf.0)))
                          ((sqrt)
                           (lambda (x) (if (>= x 0) (sqrt x) 0)))
                          ((identity)
                           (lambda (x) x))
                          (else
                           (error "Unknown transformation" transform-type))))
          
          (inverse-fn (case transform-type
                        ((log log10)
                         (lambda (y) (expt 10 y)))
                        ((log2)
                         (lambda (y) (expt 2 y)))
                        ((ln)
                         (lambda (y) (exp y)))
                        ((sqrt)
                         (lambda (y) (* y y)))
                        ((identity)
                         (lambda (y) y))
                        (else
                         (error "Unknown transformation" transform-type)))))
      
      ;; Create wrapper scale that transforms values
      (make-transformed-scale scale transform-fn inverse-fn transform-type)))

  (define (make-transformed-scale base-scale transform-fn inverse-fn trans-type)
    "Create a scale wrapper that applies transformation"
  
    ;; YASOS object that wraps the base scale
    (object
     ;; Map: apply transform, then use base scale
     ((scale-map self value)
      (let* ((domain (scale-domain base-scale))
             (d-min (car domain))
             (d-max (cdr domain))
             (t-val (transform-fn value))
             (t-min (transform-fn d-min))
             (t-max (transform-fn d-max)))
        ;; Normalize in transformed space
        (let ((normalized (/ (- t-val t-min) (- t-max t-min))))
          ;; Map through base scale's range
          (let ((range (scale-range base-scale)))
            (+ (car range) 
               (* normalized (- (cdr range) (car range))))))))

     ;; Domain: same as base
     ((scale-domain self)
      (scale-domain base-scale))

     ((scale-with-domain self new-domain)
      ;; Recursively update the base scale, then wrap again
      (make-transformed-scale
       (scale-with-domain base-scale new-domain)
       transform-fn
       inverse-fn
       trans-type))
     
     ;; Range: same as base
     ((scale-range self)
      (scale-range base-scale))
     
     ;; Train: transform values before training base scale
     ((scale-train! self values)
      (error "Cannot train a transformed scale directly"))
     
     ;; Breaks: compute in transformed space, then inverse transform
     ((scale-breaks self n)
      (let* ((domain (scale-domain base-scale))
             (d-min (car domain))
             (d-max (cdr domain))
             (t-min (transform-fn d-min))
             (t-max (transform-fn d-max))
             (t-breaks (compute-pretty-breaks t-min t-max n)))
        (map inverse-fn t-breaks)))

     
     ;; Labels: format nicely
     ((scale-labels self break-values)
      (map (lambda (v) (format "~a" v)) break-values))
     
     ;; Access to inverse function  
     ((scale-inverse self) inverse-fn)
     
     ;; Type identifier
     ((scale-type self) 'transformed)
     
     ;; Access to base scale
     ((scale-base self) base-scale)
     
     ;; Access to transform function
     ((scale-transform self) transform-fn)

     ;; Transform type
     ((scale-transform-type self) trans-type)))

  
  
  (define (scale-with-breaks scale explicit-breaks)
    "Return a new scale that uses explicit break points.
   
   Creates a wrapper that overrides automatic break computation."
    
    ;; YASOS object that wraps scale with explicit breaks
    (object
     ;; Delegate map to base scale
     ((scale-map self value)
      (scale-map scale value))
     
     ;; Delegate domain
     ((scale-domain self)
      (scale-domain scale))
     
     ;; Delegate range
     ((scale-range self)
      (scale-range scale))
     
     ;; Delegate train
     ((scale-train! self values)
      (scale-train! scale values))

     ;; Delegate inverse mapping
     ((scale-inverse self value)
      (scale-inverse scale value))

     ;; Delegate mutations — preserve wrapper around mutated base
     ((scale-set-domain! self new-domain)
      (scale-set-domain! scale new-domain))
     ((scale-set-range! self new-range)
      (scale-set-range! scale new-range))

     ;; Functional updates — re-wrap result so breaks are preserved
     ((scale-with-range self new-range)
      (scale-with-breaks (scale-with-range scale new-range) explicit-breaks))
     ((scale-with-domain self new-domain)
      (scale-with-breaks (scale-with-domain scale new-domain) explicit-breaks))

     ;; Override breaks to return explicit values (n and method args are ignored).
     ((scale-breaks self . args)
      explicit-breaks)

     ;; Labels for explicit breaks
     ((scale-labels self break-values)
      (map (lambda (v) (format "~a" v)) break-values))

     ;; Type
     ((scale-type self) 'breaks-override)

     ;; Access to base scale
     ((scale-base self) scale)

     ;; Access to explicit breaks
     ((explicit-breaks self) explicit-breaks)))
  

  (define (compute-pretty-breaks min max n)
    "Compute aesthetically pleasing break points"
  
    (if (= min max)
        (list min)
        (let* ((range (- max min))
               (rough-step (/ range (max 1 (- n 1))))
               (step (round-to-nice rough-step))
               (start (* step (floor (/ min step))))
               (end (* step (ceiling (/ max step)))))
          
          (let loop ((x start) (acc '()))
            (if (> x (+ end (* 0.5 step)))
                (reverse acc)
                (loop (+ x step) (cons x acc)))))))
  
  (define (round-to-nice x)
    "Round to nice number (1, 2, 5 multiples of powers of 10)"
  
    (if (<= x 0)
        0.1  ; Fallback for edge cases
        (let* ((exp (floor (log x 10)))
               (f (/ x (expt 10 exp)))
               (nice-f (cond
                        ((< f 1.5) 1.0)
                        ((< f 3.0) 2.0)
                        ((< f 7.0) 5.0)
                        (else 10.0))))
          (* nice-f (expt 10 exp)))))
  
  
  ;;; ========================================================================
  ;;; Color Scales
  ;;; ========================================================================

  (define (interpolate-color c1 c2 t)
    "Interpolate between two RGB colors"
    ;; Simplified: assumes colors are named strings
    ;; TODO: parse RGB and interpolate
    (if (< t 0.5) c1 c2))


;;; ------------------------------------------------------------------------
;;; Color Gradient Support
;;; ------------------------------------------------------------------------

  (define (hex-color->rgb hex-str)
    "Convert hex color string to RGB triple (0-255)"
    ;; Handle both #RRGGBB and named colors
    (cond
     ((string=? hex-str "blue") '(0 0 255))
     ((string=? hex-str "red") '(255 0 0))
     ((string=? hex-str "green") '(0 255 0))
     ((string=? hex-str "yellow") '(255 255 0))
     ((string=? hex-str "cyan") '(0 255 255))
     ((string=? hex-str "magenta") '(255 0 255))
     ((string=? hex-str "white") '(255 255 255))
     ((string=? hex-str "black") '(0 0 0))
     ((string=? hex-str "gray") '(128 128 128))
     ((string=? hex-str "grey") '(128 128 128))
     ;; Parse hex format
     ((and (string? hex-str) 
           (char=? (string-ref hex-str 0) #\#)
           (= (string-length hex-str) 7))
      (let ((r (string->number (substring hex-str 1 3) 16))
            (g (string->number (substring hex-str 3 5) 16))
            (b (string->number (substring hex-str 5 7) 16)))
        (list r g b)))
     ;; Default to black if unknown
     (else '(0 0 0))))

  (define (byte->hex-string n)
    "Convert byte (0-255) to two-digit hex string"
    (let ((hex-digits "0123456789abcdef"))
      (string
       (string-ref hex-digits (quotient n 16))
       (string-ref hex-digits (remainder n 16)))))

  (define (rgb->hex-color r g b)
    "Convert RGB triple (0-255) to hex color string"
    (let ((r-int (inexact->exact (round (min 255 (max 0 r)))))
          (g-int (inexact->exact (round (min 255 (max 0 g)))))
          (b-int (inexact->exact (round (min 255 (max 0 b))))))
      (string-append "#" 
                     (byte->hex-string r-int)
                     (byte->hex-string g-int)
                     (byte->hex-string b-int))))
  
  (define (interpolate-color low-color high-color t)
    "Interpolate between two colors
     t = 0.0 gives low-color, t = 1.0 gives high-color"
    (let ((low-rgb (hex-color->rgb low-color))
          (high-rgb (hex-color->rgb high-color)))
      (let ((r (+ (* (car low-rgb) (- 1 t)) 
                  (* (car high-rgb) t)))
            (g (+ (* (cadr low-rgb) (- 1 t)) 
                  (* (cadr high-rgb) t)))
            (b (+ (* (caddr low-rgb) (- 1 t)) 
                  (* (caddr high-rgb) t))))
        (rgb->hex-color r g b))))


  
  (define (make-scale-color-gradient #!key domain range (low "white") (high "blue"))
    "Continuous color scale that interpolates between low and high colors
   
     Returns a scale that:
     - Maps numeric values to interpolated hex color strings
     - Can be trained on data to determine domain
     - Integrates with the standard scale protocol"
  
    (let ((numeric-scale (make-scale-linear #:domain domain #:range '(0 . 1))))
      
      (object
       ((scale? self) #t)
       
       ((scale-type self) 'color-gradient)
       
       ;; Training - delegate to underlying numeric scale
       ((scale-train! self values)
        (scale-train! numeric-scale values))
       
       ;; Mapping - normalize to [0,1], then interpolate color
       ((scale-map self value)
        (if (number? value)
            (let* ((t (scale-map numeric-scale value))
                   ;; Clamp to [0, 1] range
                   (clamped (min 1.0 (max 0.0 t))))
              (interpolate-color low high clamped))
            ;; Return default if not a number
            low))
       
       ;; Domain access - delegate to numeric scale
       ((scale-domain self)
        (scale-domain numeric-scale))
       
       ;; Range access - return color endpoints
       ((scale-range self)
        (cons low high))
       
       ;; Set domain - delegate to numeric scale
       ((scale-set-domain! self new-domain)
        (scale-set-domain! numeric-scale new-domain))
       
       ;; Set range - update colors (note: range here means colors)
       ((scale-set-range! self new-range)
        ;; For color gradient, range is the color pair
        ;; This is a bit awkward but maintains API
        (error "Use scale-with-colors for gradient color changes"))
       
       ;; Functional domain update
       ((scale-with-domain self new-domain)
        (make-scale-color-gradient #:domain new-domain 
                                   #:low low 
                                   #:high high))
       
       ;; Functional range update (colors)
       ((scale-with-range self new-range)
        ;; For gradient, interpret as (low . high) colors
        (make-scale-color-gradient #:domain (scale-domain numeric-scale)
                                   #:low (car new-range)
                                   #:high (cdr new-range)))
       
       ;; Breaks - delegate to numeric scale
       ((scale-breaks self . args)
        (apply scale-breaks numeric-scale args))
       
       ;; Labels - delegate to numeric scale
       ((scale-labels self breaks)
        (scale-labels numeric-scale breaks))
       
       ;; Inverse - map color back to value (difficult, not implemented)
       ((scale-inverse self color)
        (error "Inverse mapping not supported for color gradients"))
       )))
  
  
  (define (make-scale-color-diverging 
           #!key domain range (low "blue") (mid "white") (high "red") (midpoint 0))
    "Diverging color scale with midpoint"
    (let ((base-scale (make-scale-linear #:domain domain #:range '(0 . 1))))
      (object-with-ancestors ((base-scale base-scale))
       ((scale-map self value)
        (let ((t (scale-map base-scale value)))
          (cond
           ((< value midpoint) (interpolate-color low mid 
                                                 (/ (- value (car (scale-domain base-scale)))
                                                    (- midpoint (car (scale-domain base-scale))))))
           ((> value midpoint) (interpolate-color mid high 
                                                 (/ (- value midpoint)
                                                    (- (cdr (scale-domain base-scale)) midpoint))))
           (else mid)))))))

  (define (make-scale-color-ordinal #!key domain (palette '("red" "blue" "green")))
    "Ordinal color scale with palette"
    (make-scale-ordinal #:domain domain #:range palette))

  (define (make-scale-color-manual value-map)
    "Create a manual color scale with explicit category -> color mapping.
   
     value-map: Alist mapping category values to colors
                '((\"Control\" . \"red\") (\"Treatment\" . \"blue\"))
   
     This is a discrete scale that maps categories directly to colors,
     not through a continuous range."
  
    (let ((mapping (make-hash-table equal?))
          (domain '()))
      
      ;; Initialize mapping from value-map
      (for-each (lambda (pair)
                  (hash-table-set! mapping (car pair) (cdr pair)))
                value-map)
      
      ;; Extract domain from value-map
      (set! domain (map car value-map))
      
      (object
       ;; Type identifier
       ((scale-type self) 'color-manual)
       
       ;; Domain (list of category values)
       ((scale-domain self) domain)
       
       ;; Set domain (update mapping keys if needed)
       ((scale-set-domain! self new-domain)
        (set! domain new-domain))
       
       ;; Range is the list of colors
       ((scale-range self) 
        (map (lambda (key) (hash-table-ref/default mapping key "gray"))
             domain))
       
       ;; No breaks override for manual scales
       ((scale-breaks-override self) #f)
       ((scale-set-breaks-override! self breaks) (void))
       
       ;; Train: add new categories with default color
       ((scale-train! self values)
        (for-each (lambda (val)
                    (unless (member val domain)
                      (set! domain (append domain (list val)))
                      ;; Assign default color if not in mapping
                      (unless (hash-table-exists? mapping val)
                        (hash-table-set! mapping val "gray"))))
                  (delete-duplicates values)))
     
       ;; Map: category -> color (direct lookup)
       ((scale-map self value)
        (hash-table-ref/default mapping value "gray"))
     
       ;; Breaks: return all categories
       ((scale-breaks self n)
        domain)
       
       ;; Labels: format categories
       ((scale-labels self break-values)
        (map (lambda (v) (format "~a" v)) break-values))
       
       ;; Get color for specific value
       ((scale-color-for self value)
        (hash-table-ref/default mapping value "gray"))
       
       ;; Set color for specific value
       ((scale-set-color! self value color)
        (hash-table-set! mapping value color)
        (unless (member value domain)
          (set! domain (append domain (list value))))))))

  
  
  ;;; ========================================================================
  ;;; Utility Functions
  ;;; ========================================================================

  (define (nice-number x round?)
    "Find a 'nice' number close to x (Heckbert {1, 2, 5} set)"
    (let* ((exp (floor (log x 10)))
           (f (/ x (expt 10 exp)))
           (nf (cond
                ((and round? (<= f 1)) 1.0)
                ((and round? (<= f 2)) 2.0)
                ((and round? (<= f 5)) 5.0)
                ((and round? (<= f 10)) 10.0)
                ((<= f 1.5) 1.0)
                ((<= f 3) 2.0)
                ((<= f 7) 5.0)
                (else 10.0))))
      (* nf (expt 10 exp))))

  (define (nice-step-size raw-step)
    "Select a step from {1, 2, 2.5, 5} × 10^n using geometric-midpoint boundaries.
    Boundaries: sqrt(1*2) ~= 1.41, sqrt(2*2.5) ~= 2.24, sqrt(2.5*5) ~= 3.54, sqrt(5*10) ~= 7.07"
    (if (<= raw-step 0)
        0.1
        (let* ((exp (floor (log raw-step 10)))
               (f   (/ raw-step (expt 10 exp)))
               (nf  (cond ((< f (sqrt 2.0))   1.0)
                          ((< f (sqrt 5.0))   2.0)
                          ((< f (sqrt 12.5))  2.5)
                          ((< f (sqrt 50.0))  5.0)
                          (else              10.0))))
          (* nf (expt 10 exp)))))

  (define (nice-breaks-heckbert min-val max-val n)
    "Compute nice break positions using Heckbert {1, 2, 5} step set (loose labelling)."
    (if (= min-val max-val)
        (list min-val)
        (let* ((range (nice-number (- max-val min-val) #f))
               (tick-spacing (nice-number (/ range (- n 1)) #t))
               (nice-min (* (floor (/ min-val tick-spacing)) tick-spacing))
               (nice-max (* (ceiling (/ max-val tick-spacing)) tick-spacing)))
          (let loop ((x nice-min) (result '()))
            (if (> x nice-max)
                (reverse result)
                (loop (+ x tick-spacing) (cons x result)))))))

  (define (nice-breaks min-val max-val n)
    "Compute nice break positions using {1, 2, 2.5, 5} step sizes (R pretty() style).

    Loose labelling: nice-min <= min-val, nice-max >= max-val.
    min.n guard: if the initial step yields fewer than floor(n/3) intervals,
    halve the candidate step and retry via nice-step-size."
    (if (= min-val max-val)
        (list min-val)
        (let* ((raw-step  (/ (- max-val min-val) (- n 1)))
               (min-ticks (max 1 (floor (/ n 3))))
               (step      (let lp ((s (nice-step-size raw-step)))
                            (let* ((lo (* (floor   (/ min-val s)) s))
                                   (hi (* (ceiling (/ max-val s)) s))
                                   (k  (round (/ (- hi lo) s))))
                              (if (>= k min-ticks)
                                  s
                                  (lp (nice-step-size (/ s 2)))))))
               (nice-min  (* (floor   (/ min-val step)) step))
               (nice-max  (* (ceiling (/ max-val step)) step)))
          (let loop ((x nice-min) (result '()))
            (if (> x (+ nice-max (* step 1e-10)))
                (reverse result)
                (loop (+ x step) (cons x result)))))))

  (define (extend-domain domain expansion)
    "Extend domain by a fractional amount (e.g., 0.05 for 5% padding)"
    (let* ((min-val (car domain))
           (max-val (cdr domain))
           (range (- max-val min-val))
           (padding (* range expansion)))
      (cons (- min-val padding) (+ max-val padding))))

) ; end module
