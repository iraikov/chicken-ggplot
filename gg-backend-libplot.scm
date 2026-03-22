;; gg-backend-libplot.scm
;; libplot wrapper backend for the Grammar of Graphics library.
;;
;; Provides a gg-backend-protocol-compliant object wrapping the existing
;; CHICKEN `plot` egg (GNU libplot bindings).  This exists to allow the
;; rendering pipeline in gg-primitives.scm and gg-plot.scm to be migrated
;; to the backend abstraction without breaking the libplot code path.
;;
;; Known limitations:
;;
;; - No transparency: alpha components of colors are silently ignored.
;;
;; - Font rendering quality depends on how the `plot` egg was compiled.
;;   Standard Debian/Ubuntu packages only provide
;;   Hershey vector fonts rather than FreeType-rendered system fonts.
;;
;; - Clipping (backend/set-clip-rect!) is not supported; calls are no-ops.
;;
;; - backend/set-dash! is not implemented; calls are no-ops.
;;
;; - libplot's savestate/restorestate does NOT save the fspace coordinate
;;   transform.  We re-apply (fspace) after every pop-state! call.
;;
;; - backend/text-extents returns heuristic estimates (not measured).
;;
;; - backend/get-width / backend/get-height return the values supplied
;;   at construction time; libplot does not expose them at runtime.

(module gg-backend-libplot
  (make-libplot-png-backend
   make-libplot-svg-backend
   make-libplot-ps-backend
   make-libplot-generic-backend)

(import scheme (chicken base) (chicken string)
        yasos
        gg-backend
        plot)   ;; GNU libplot bindings

;;; ================================================================
;;; Color conversion: `color` -> libplot integer RGB (0–65535)
;;; ================================================================

(define (color->pl-rgb col)
  "Returns (values r16 g16 b16).  Alpha is discarded."
  (call-with-values
    (lambda () (color->rgba-values col))
    (lambda (r g b _a)
      (values (inexact->exact (round (* r 65535.0)))
              (inexact->exact (round (* g 65535.0)))
              (inexact->exact (round (* b 65535.0)))))))

;;; ================================================================
;;; libplot viewport record
;;; We store the fspace parameters and reapply after pop-state!,
;;; because libplot savestate does NOT save the coordinate transform.
;;; ================================================================

(define-record-type <pl-viewport>
  (make-pl-vp x0 y0 x1 y1)
  pl-vp?
  (x0 pl-vp-x0) (y0 pl-vp-y0)
  (x1 pl-vp-x1) (y1 pl-vp-y1))

(define (pl-vp-translate vp dx dy)
  (make-pl-vp (+ (pl-vp-x0 vp) dx) (+ (pl-vp-y0 vp) dy)
              (+ (pl-vp-x1 vp) dx) (+ (pl-vp-y1 vp) dy)))

;;; Apply viewport to plotter via (fspace).
(define (pl-apply-vp! plotter vp)
  (fspace plotter
    (pl-vp-x0 vp) (pl-vp-y0 vp)
    (pl-vp-x1 vp) (pl-vp-y1 vp)))

;;; ================================================================
;;; Draw-state record
;;; ================================================================

(define-record-type <pl-draw-state>
  (%make-pl-ds viewport pen fill font-name font-size)
  pl-draw-state?
  (viewport  pl-ds-viewport  pl-ds-viewport-set!)
  (pen       pl-ds-pen       pl-ds-pen-set!)
  (fill      pl-ds-fill      pl-ds-fill-set!)
  (font-name pl-ds-font-name pl-ds-font-name-set!)
  (font-size pl-ds-font-size pl-ds-font-size-set!))

(define (copy-pl-ds ds)
  (%make-pl-ds (pl-ds-viewport  ds) (pl-ds-pen      ds)
               (pl-ds-fill      ds) (pl-ds-font-name ds)
               (pl-ds-font-size ds)))

(define (make-default-pl-ds)
  (%make-pl-ds (make-pl-vp 0.0 0.0 1000.0 1000.0)
               color-black color-white
               "HersheySerif" 12.0))

;;; ================================================================
;;; Backend object
;;; ================================================================

(define (make-libplot-generic-backend make-plotter-thunk width height)
  "Low-level constructor.  make-plotter-thunk is a zero-argument
   procedure that returns an un-opened libplot plotter."
  (let ((plotter #f)
        (ds      #f)
        (stack   '())   ;; list of saved pl-draw-state
        (w*      (exact->inexact width))
        (h*      (exact->inexact height)))

    ;; Helpers
    (define (apply-pen! col)
      (call-with-values (lambda () (color->pl-rgb col))
        (lambda (r g b) (pencolor plotter r g b))))
    (define (apply-fill! col)
      (call-with-values (lambda () (color->pl-rgb col))
        (lambda (r g b) (fillcolor plotter r g b))))

    (object
      ((graphics-backend? self) #t)

      ;; Lifecycle

      ((backend/open! self)
       (set! plotter (make-plotter-thunk))
       (openpl plotter)
       (set! ds    (make-default-pl-ds))
       (set! stack '())
       (pl-apply-vp! plotter (pl-ds-viewport ds))
       (apply-pen!  (pl-ds-pen  ds))
       (apply-fill! (pl-ds-fill ds)))

      ((backend/close! self)
       (when plotter
         (closepl plotter)
         (delete-plotter plotter)
         (set! plotter #f)
         (set! ds #f)))

      ;; State stack
      ;;
      ;; libplot savestate saves drawing attributes (colors, line width,
      ;; font, fill type) but NOT the fspace coordinate transform.
      ;; We save both and re-apply fspace after restorestate.

      ((backend/push-state! self)
       (savestate plotter)
       (set! stack (cons (copy-pl-ds ds) stack)))

      ((backend/pop-state! self)
       (when (null? stack)
         (error "backend/pop-state!: state stack underflow"))
       (restorestate plotter)
       (set! ds    (car stack))
       (set! stack (cdr stack))
       ;; Re-establish coordinate space (not saved by libplot)
       (pl-apply-vp! plotter (pl-ds-viewport ds)))

      ;; Viewport

      ((backend/set-viewport! self x0 y0 x1 y1)
       (let ((vp (make-pl-vp (exact->inexact x0) (exact->inexact y0)
                              (exact->inexact x1) (exact->inexact y1))))
         (pl-ds-viewport-set! ds vp)
         (pl-apply-vp! plotter vp)))

      ((backend/translate! self dx dy)
       (let ((vp (pl-vp-translate (pl-ds-viewport ds)
                                  (exact->inexact dx)
                                  (exact->inexact dy))))
         (pl-ds-viewport-set! ds vp)
         (pl-apply-vp! plotter vp)))

      ;; Clipping
      ;; libplot has no explicit clip rectangle API; treat as no-op.

      ((backend/set-clip-rect! self x y w h) (void))
      ((backend/reset-clip! self)            (void))

      ;; Style

      ((backend/set-pen-color! self col)
       (let ((c (parse-color col)))
         (pl-ds-pen-set! ds c)
         (apply-pen! c)))

      ((backend/set-fill-color! self col)
       (let ((c (parse-color col)))
         (pl-ds-fill-set! ds c)
         (apply-fill! c)))

      ((backend/set-line-width! self w)
       (linewidth plotter (exact->inexact w)))

      ;; libplot dash support is limited; no-op for now.
      ((backend/set-dash! self dashes offset) (void))

      ;; libplot font: slant and weight are encoded in the font name.
      ;; We store the family for text-extents estimation and use fontname.
      ((backend/set-font! self family size slant weight)
       (let ((name (match (list slant weight)
                     (('italic 'bold)   (string-append family "-BoldItalic"))
                     (('italic _)       (string-append family "-Italic"))
                     ((_ 'bold)         (string-append family "-Bold"))
                     (_                 family))))
         (pl-ds-font-name-set! ds name)
         (pl-ds-font-size-set! ds (exact->inexact size))
         (fontname plotter name)
         (fontsize plotter (exact->inexact size))))

      ;; Stroked primitives

      ((backend/draw-line! self x1 y1 x2 y2)
       (apply-pen! (pl-ds-pen ds))
       (fline plotter
         (exact->inexact x1) (exact->inexact y1)
         (exact->inexact x2) (exact->inexact y2)))

      ((backend/draw-polyline! self pts)
       (when (pair? pts)
         (apply-pen! (pl-ds-pen ds))
         (fmove plotter (exact->inexact (caar pts)) (exact->inexact (cdar pts)))
         (for-each
           (lambda (pt)
             (fcont plotter (exact->inexact (car pt)) (exact->inexact (cdr pt))))
           (cdr pts))
         (endpath plotter)))

      ((backend/draw-rect! self x y w h)
       (apply-pen! (pl-ds-pen ds))
       (filltype plotter 0)
       ;; libplot fbox takes two corners, not (x y w h)
       (fbox plotter
         (exact->inexact x)       (exact->inexact y)
         (exact->inexact (+ x w)) (exact->inexact (+ y h))))

      ((backend/draw-circle! self cx cy r)
       (apply-pen! (pl-ds-pen ds))
       (filltype plotter 0)
       (fcircle plotter
         (exact->inexact cx) (exact->inexact cy) (exact->inexact r)))

      ((backend/draw-polygon! self pts)
       (when (pair? pts)
         (apply-pen! (pl-ds-pen ds))
         (filltype plotter 0)
         (fmove plotter (exact->inexact (caar pts)) (exact->inexact (cdar pts)))
         (for-each
           (lambda (pt)
             (fcont plotter (exact->inexact (car pt)) (exact->inexact (cdr pt))))
           (cdr pts))
         (endpath plotter)))

      ;; ── Filled primitives ────────────────────────────────────

      ((backend/draw-filled-rect! self x y w h)
       ;; Fill pass
       (apply-fill! (pl-ds-fill ds))
       (filltype plotter 1)
       (fbox plotter
         (exact->inexact x)       (exact->inexact y)
         (exact->inexact (+ x w)) (exact->inexact (+ y h)))
       ;; Stroke pass
       (apply-pen! (pl-ds-pen ds))
       (filltype plotter 0)
       (fbox plotter
         (exact->inexact x)       (exact->inexact y)
         (exact->inexact (+ x w)) (exact->inexact (+ y h))))

      ((backend/draw-filled-circle! self cx cy r)
       (apply-fill! (pl-ds-fill ds))
       (filltype plotter 1)
       (fcircle plotter
         (exact->inexact cx) (exact->inexact cy) (exact->inexact r)))

      ((backend/draw-filled-polygon! self pts)
       (when (pair? pts)
         (apply-fill! (pl-ds-fill ds))
         (filltype plotter 1)
         (fmove plotter (exact->inexact (caar pts)) (exact->inexact (cdar pts)))
         (for-each
           (lambda (pt)
             (fcont plotter (exact->inexact (car pt)) (exact->inexact (cdr pt))))
           (cdr pts))
         (endpath plotter)))

      ;; ── General path ─────────────────────────────────────────
      ;; libplot paths use fmove/fcont/endpath.
      ;; path:curve-to and path:arc are not natively supported;
      ;; they are silently skipped.  Upgrade to Cairo for these.

      ((backend/draw-path! self cmds filled?)
       (when filled? (apply-fill! (pl-ds-fill ds)) (filltype plotter 1))
       (apply-pen! (pl-ds-pen ds))
       (let loop ((cmds cmds) (open? #f))
         (if (null? cmds)
             (when open? (endpath plotter))
             (cases path-cmd (car cmds)
               ((path:move-to x y)
                (when open? (endpath plotter))
                (fmove plotter (exact->inexact x) (exact->inexact y))
                (loop (cdr cmds) #t))
               ((path:line-to x y)
                (fcont plotter (exact->inexact x) (exact->inexact y))
                (loop (cdr cmds) #t))
               ((path:close)
                (endpath plotter)
                (loop (cdr cmds) #f))
               (else
                ;; curve-to / arc: not supported in libplot; skip
                (loop (cdr cmds) open?))))))

      ;; Text
      ;; libplot places text at the current pen position via (label).
      ;; Horizontal alignment is approximate; (textangle) supports rotation.
      ;; Vertical alignment below baseline is not available.

      ((backend/draw-text! self x y text halign valign)
       (apply-pen! (pl-ds-pen ds))
       (fmove plotter (exact->inexact x) (exact->inexact y))
       (label plotter text))

      ;; text-extents: heuristic only (libplot has no query API)
      ((backend/text-extents self text)
       (let* ((size (pl-ds-font-size ds))
              (w    (* (string-length text) size 0.60))
              (h    (* size 1.2))
              (asc  (* size 0.8))
              (dsc  (* size 0.2)))
         (values w h asc dsc)))

      ;; Dimensions

      ((backend/get-width  self) w*)
      ((backend/get-height self) h*))))

;;; Convenience constructors

(define (make-libplot-png-backend port width height)
  (make-libplot-generic-backend
    (lambda ()
      (make-plotter (PNG) port
        (list (BITMAPSIZE
                (string-append
                  (number->string (inexact->exact (round width)))
                  "x"
                  (number->string (inexact->exact (round height))))))))
    width height))

(define (make-libplot-svg-backend port width height)
  (make-libplot-generic-backend
    (lambda () (make-plotter (SVG) port '()))
    width height))

(define (make-libplot-ps-backend port width height)
  (make-libplot-generic-backend
    (lambda () (make-plotter (PS) port '()))
    width height))

) ;; end module gg-backend-libplot
