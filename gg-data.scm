;;;; gg-data.scm
;;;; Grammar of Graphics - Data Utilities
;;;;
;;;; Utilities for working with columnar data

(module gg-data
  (;; Data frame operations
   data-column
   data-n-rows
   data-columns
   data-row
   data-rows
   
   ;; Data conversion
   columns->rows
   rows->columns)

  (import scheme
          (chicken base)
          (chicken format)
          srfi-1)
          

  ;;; ========================================================================
  ;;; Data Format
  ;;; ========================================================================
  ;;;
  ;;; Data is represented as an association list of columns:
  ;;;   '((column-name . (values...))
  ;;;     (column-name . (values...))
  ;;;     ...)
  ;;;
  ;;; Example:
  ;;;   '((x . (1 2 3 4))
  ;;;     (y . (10 20 30 40))
  ;;;     (group . ("A" "A" "B" "B")))

  ;;; ========================================================================
  ;;; Column Operations
  ;;; ========================================================================

  (define (data-column data column-name)
    "Extract a column by name"
    (let ((entry (assoc column-name data)))
      (if entry
          (cdr entry)
          (error (sprintf "Column not found: ~a" column-name)))))

  (define (data-columns data)
    "Get list of column names"
    (map car data))

  (define (data-n-rows data)
    "Get number of rows in data"
    (if (null? data)
        0
        (length (cdar data))))

  ;;; ========================================================================
  ;;; Row Operations
  ;;; ========================================================================

  (define (data-row data index)
    "Extract a single row as an association list"
    (map (lambda (col)
           (cons (car col)
                 (list-ref (cdr col) index)))
         data))

  (define (data-rows data)
    "Convert columnar data to list of row association lists"
    (let ((n (data-n-rows data)))
      (map (lambda (i) (data-row data i))
           (iota n))))

  ;;; ========================================================================
  ;;; Data Conversion
  ;;; ========================================================================

  (define (columns->rows data)
    "Convert columnar format to row format"
    (data-rows data))

  (define (rows->columns rows)
    "Convert row format to columnar format"
    (if (null? rows)
        '()
        (let ((column-names (map car (car rows))))
          (map (lambda (col-name)
                 (cons col-name
                       (map (lambda (row)
                              (cdr (assoc col-name row)))
                            rows)))
               column-names))))

) ; end module
