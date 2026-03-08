;;;; gg-aes.scm
;;;; Grammar of Graphics - Aesthetic Mapping
;;;;
;;;; Maps data columns to visual properties (position, color, size, etc.)

(module gg-aes
  (;; Aesthetic creation
   make-aes
   aes
   aesthetic-mapping?
   
   ;; Aesthetic accessors
   aes-x
   aes-y
   aes-color
   aes-fill
   aes-get
   aes-size
   aes-shape
   aes-alpha
   aes-linetype
   aes-group
   aes-label
   
   ;; Aesthetic utilities
   aes-merge
   aes-has?
   aes-map-value
   aes-keys
   
   ;; Constants vs mappings
   constant-value?
   mapped-value?)

  (import scheme
          (chicken base)
          (chicken keyword)
          srfi-1
          srfi-69)  ; Hash tables

  ;;; ========================================================================
  ;;; Aesthetic Mapping
  ;;; ========================================================================
  ;;;
  ;;; An aesthetic mapping specifies how data columns map to visual properties.
  ;;; Aesthetics can be:
  ;;;   - Mapped: A symbol referring to a data column (e.g., 'time)
  ;;;   - Constant: A literal value (e.g., "red", 5)
  ;;;
  ;;; Example:
  ;;;   (aes #:x 'time #:y 'value #:color "red")
  ;;;   - x and y are mapped to data columns
  ;;;   - color is a constant

  (define-record-type aesthetic-mapping
    (make-aesthetic-mapping-internal mappings)
    aesthetic-mapping?
    (mappings aes-mappings))

  (define (make-aes . args)
    "Create an aesthetic mapping from keyword arguments"
    (let ((mappings (make-hash-table)))
      (let loop ((args args))
        (if (< (length args) 2)
            (make-aesthetic-mapping-internal mappings)
            (let ((key (car args))
                  (value (cadr args)))
              ;; Convert keyword to symbol (remove leading #:)
              (let ((aes-name (string->symbol 
                               (keyword->string key) )))
                (hash-table-set! mappings aes-name value)
                (loop (cddr args))))))))

  ;; Alias for make-aes
  (define aes make-aes)

  ;;; ========================================================================
  ;;; Aesthetic Accessors
  ;;; ========================================================================

  (define (aes-get aesthetic-map key #!optional default)
    "Get value for aesthetic key"
    (hash-table-ref/default (aes-mappings aesthetic-map) key default))

  (define (aes-x aes) (aes-get aes 'x #f))
  (define (aes-y aes) (aes-get aes 'y #f))
  (define (aes-color aes) (aes-get aes 'color #f))
  (define (aes-fill aes) (aes-get aes 'fill #f))
  (define (aes-size aes) (aes-get aes 'size #f))
  (define (aes-shape aes) (aes-get aes 'shape #f))
  (define (aes-alpha aes) (aes-get aes 'alpha #f))
  (define (aes-linetype aes) (aes-get aes 'linetype #f))
  (define (aes-group aes) (aes-get aes 'group #f))
  (define (aes-label aes) (aes-get aes 'label #f))

  ;;; ========================================================================
  ;;; Aesthetic Utilities
  ;;; ========================================================================

  (define (aes-merge base-aes override-aes)
    "Merge two aesthetic mappings, with override taking precedence"
    (let ((new-mappings (make-hash-table)))
      ;; Copy base mappings
      (hash-table-walk (aes-mappings base-aes)
                      (lambda (k v) (hash-table-set! new-mappings k v)))
      ;; Override with new mappings
      (hash-table-walk (aes-mappings override-aes)
                      (lambda (k v) (hash-table-set! new-mappings k v)))
      (make-aesthetic-mapping-internal new-mappings)))

  (define (aes-has? aesthetic-map key)
    "Check if aesthetic mapping has a key"
    (hash-table-exists? (aes-mappings aesthetic-map) key))

  (define (aes-keys aesthetic-map)
    "Get list of all aesthetic keys"
    (hash-table-keys (aes-mappings aesthetic-map)))

  (define (aes-map-value aesthetic-map key data-row)
    "Extract value for aesthetic from data row
     - If aesthetic is mapped (symbol), extract from data row
     - If aesthetic is constant, return the constant"
    (let ((aes-value (aes-get aesthetic-map key #f)))
      (cond
       ((not aes-value) #f)
       ((symbol? aes-value)  ; Mapped to column
        (cond
         ((list? data-row)    ; Association list
          (let ((entry (assoc aes-value data-row)))
            (if entry (cdr entry) #f)))
         ((vector? data-row)  ; Vector (index by position)
          #f)  ; Not implemented yet
         (else #f)))
       (else aes-value))))  ; Constant value

  ;;; ========================================================================
  ;;; Constants vs Mappings
  ;;; ========================================================================

  (define (constant-value? value)
    "Check if aesthetic value is a constant (not a column mapping)"
    (not (symbol? value)))

  (define (mapped-value? value)
    "Check if aesthetic value is a column mapping"
    (symbol? value))

) ; end module
