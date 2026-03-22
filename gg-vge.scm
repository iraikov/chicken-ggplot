;; gg-vge.scm
;; Virtual Graphics Engine for the gg Grammar of Graphics library.
;;
;; This module introduces an intermediate representation (IR) layer
;; between the high-level drawing combinators (gg-primitives) and the
;; concrete graphics backends (gg-backend-cairo, gg-backend-libplot).
;;
;; The three-layer pipeline:
;;
;;   EMIT phase:  drawers call (vge-emit! vge insn)
;;                - accumulates gfx-insn values into a <vge>
;;
;;   OPTIMIZE phase (optional): (vge-optimize! vge)
;;                - rewrites the instruction tree in-place
;;
;;   RENDER phase: (vge-render! vge backend) or (vge-render-to! vge backend)
;;                - walks the gfx-insn tree, dispatches to backend operations
;;
;; The intermediate representation is a tree:
;; gfx:group nests a sub-sequence with implicit push/pop state, thus enabling:
;;   - Structural inspection without simulating a stack
;;   - Optimization passes that operate on sub-trees
;;   - Serialization / deserialization of plot specifications
;;   - Bounding-box queries without opening a surface
;;
;; Coordinate and color conventions
;; ---------------------------------
;; All coordinates follow the gg-backend convention: right-handed user space,
;; origin at bottom-left, Y-up.  Colors are `color` values from gg-backend.

(module gg-vge
  ( ;; Instruction type
    ;; Constructors (algebraic datatype variants)
    gfx:set-viewport
    gfx:translate
    gfx:set-clip-rect
    gfx:reset-clip
    gfx:set-pen-color
    gfx:set-fill-color
    gfx:set-line-width
    gfx:set-dash
    gfx:set-font
    gfx:draw-line
    gfx:draw-polyline
    gfx:draw-rect
    gfx:draw-circle
    gfx:draw-polygon
    gfx:draw-filled-rect
    gfx:draw-filled-circle
    gfx:draw-filled-polygon
    gfx:draw-path
    gfx:draw-text
    gfx:group ; nested sequence with implicit push/pop
    ;; Type predicate and case-analysis accessor
    gfx-insn?
    gfx-insn-cases

    ;; VGE insn accumulator
    make-vge
    vge?
    vge-emit!          ; (vge-emit! vge insn)
    vge-emit-all!      ; (vge-emit-all! vge list-of-insns)
    vge-group!         ; (vge-group! vge thunk) -> emits group wrapping thunk
    vge-instructions   ; -> snapshot as list (preserved order)
    vge-clear!

    ;;  Rendering
    vge-render-to!     ; render to an already-open backend
    vge-render!        ; open backend, render, close

    ;; Utilities
    drawer->instructions  ; extract flat instruction list from a drawer
    vge-optimize          ; -> new list of gfx-insn with peephole opts applied
    print-gfx-insn     ; for debugging / pretty-printing
  )

  (import scheme
          (chicken base)
          (chicken pretty-print)
          srfi-1
          datatype 
          gg-backend)   ;; color?, path-cmd?, halign/*, valign/*, backend operations

;;; ================================================================
;;; gfx-insn algebraic datatype
;;; ================================================================
;;;
;;; Each variant corresponds exactly to one gg-backend protocol operation,
;;; except gfx:group which corresponds to a push-state / body / pop-state
;;; triple.  

(define-datatype gfx-insn gfx-insn?

  ;; Viewport & transform
  ;; Analogous to libplot's (fspace plotter x0 y0 x1 y1)
  (gfx:set-viewport
    (x0 real?) (y0 real?) (x1 real?) (y1 real?))

  ;; Accumulate a translation into the current viewport.
  (gfx:translate
    (dx real?) (dy real?))

  ;; Clipping
  ;; (x, y) = bottom-left of clip rectangle in user space.
  (gfx:set-clip-rect
    (x real?) (y real?) (w real?) (h real?))

  (gfx:reset-clip)

  ;; Style state
  ;; Each instruction sets one graphical property on the backend.
  (gfx:set-pen-color   (col color?))
  (gfx:set-fill-color  (col color?))
  (gfx:set-line-width  (w real?))

  ;; dashes  = list of alternating on/off lengths in device pixels
  ;; offset  = phase offset
  ;; Pass (gfx:set-dash '() 0.0) to restore solid line.
  (gfx:set-dash        (dashes list?) (offset real?))

  ;; slant   : {normal italic oblique}
  ;; weight  : {normal bold}
  ;; size    is in device points (not user coordinates)
  (gfx:set-font        (family string?) (size real?)
                       (slant symbol?)  (weight symbol?))

  ;; Stroked drawing primitives
  (gfx:draw-line       (x1 real?) (y1 real?) (x2 real?) (y2 real?))

  ;; pts = list of (x . y) pairs
  (gfx:draw-polyline   (pts list?))

  ;; (x, y) = bottom-left corner; w, h > 0
  (gfx:draw-rect       (x real?) (y real?) (w real?) (h real?))

  ;; Radius in user coordinates (X axis scale)
  (gfx:draw-circle     (cx real?) (cy real?) (r real?))

  ;; pts = list of (x . y) pairs; path is implicitly closed
  (gfx:draw-polygon    (pts list?))

  ;; Filled drawing primitives
  ;; Each fills with current fill color, then strokes with pen color.
  ;; Set pen color to color-transparent to suppress outline.
  (gfx:draw-filled-rect    (x real?) (y real?) (w real?) (h real?))
  (gfx:draw-filled-circle  (cx real?) (cy real?) (r real?))
  (gfx:draw-filled-polygon (pts list?))

  ;; General path
  ;; cmds    = list of path-cmd values (from gg-backend)
  ;; filled? = if #t, fill then stroke; else stroke only
  (gfx:draw-path       (cmds list?) (filled? boolean?))

  ;; Text
  ;; (x, y) is the anchor point in user space.
  ;; halign: {halign/left halign/center halign/right}
  ;; valign: {valign/top valign/center valign/bottom valign/baseline}
  (gfx:draw-text       (x real?) (y real?) (text string?)
                       (halign symbol?) (valign symbol?))

  ;; Group (nested scope with implicit push/pop)
  ;; body = list of gfx-insn values
  ;; At render time: push-state, render body, pop-state.
  ;; The tree structure makes the scope boundary explicit in the IR,
  ;; avoiding the need to simulate a stack during analysis passes.
  (gfx:group           (body list?))
)

;;; ================================================================
;;; gfx-insn-cases: a matchable-style dispatch macro helper
;;; ================================================================
;;;
;;; Usage pattern in non-cases contexts (e.g. optimizers):
;;;   (gfx-insn-cases insn
;;;     (draw? (gfx:draw-line _ _ _ _) #t)
;;;     (draw? (gfx:draw-filled-rect _ _ _ _) #t)
;;;     (else  #f))
;;; We just re-export `cases` from datatype under an alias so the
;;; module is self-contained and callers don't have to import datatype.

(define-syntax gfx-insn-cases
  (syntax-rules ()
    ((_ e clause ...) (cases gfx-insn e clause ...))))

;;; ================================================================
;;; <vge>: Virtual Graphics Engine accumulator
;;;
;;; A VGE is a mutable accumulator that collects gfx-insn values
;;; emitted by drawers.  It stores instructions in reverse order
;;; internally and reverses on read (O(1) emit, O(n) snapshot).
;;; ================================================================

(define-record-type <vge>
  (%make-vge acc-cell)
  vge?
  ;; acc-cell is a mutable wrapper: (list reversed-insns)
  (acc-cell vge-acc-cell))

(define (make-vge)
  (%make-vge (list '())))

;;; Emit a single instruction into the VGE.
;;; O(1) amortised.
(define (vge-emit! vge insn)
  (let ((cell (vge-acc-cell vge)))
    (set-car! cell (cons insn (car cell)))))

;;; Emit a pre-built list of instructions in order.
(define (vge-emit-all! vge insns)
  (for-each (lambda (i) (vge-emit! vge i)) insns))

;;; Emit a gfx:group whose body is the set of instructions
;;; accumulated by calling thunk.  Each sub-draw is a
;;; nested tree node.
;;;
;;; Example:
;;;   (vge-group! vge
;;;     (lambda ()
;;;       (vge-emit! vge (gfx:set-pen-color (parse-color "red")))
;;;       (vge-emit! vge (gfx:draw-line 0 0 100 100))))
;;;
;;; Results in: (gfx:group ((gfx:set-pen-color ...) (gfx:draw-line ...)))
;;; appended to vge.
(define (vge-group! vge thunk)
  (let ((inner (make-vge)))
    (thunk inner)
    (vge-emit! vge (gfx:group (vge-instructions inner)))))

;;; Return the accumulated instructions as an ordered list.
;;; Creates a fresh copy (snapshot); does not clear the accumulator.
(define (vge-instructions vge)
  (reverse (car (vge-acc-cell vge))))

;;; Discard all accumulated instructions.
(define (vge-clear! vge)
  (set-car! (vge-acc-cell vge) '()))

;;; ================================================================
;;; Instruction interpreter
;;; ================================================================
;;;
;;; eval-insn! dispatches a single gfx-insn to the
;;; corresponding backend operation.  It calls itself recursively
;;; on gfx:group bodies.

(define (eval-insn! insn backend)
  (cases gfx-insn insn

    ;; Viewport & transform
    (gfx:set-viewport (x0 y0 x1 y1)
     (backend/set-viewport! backend x0 y0 x1 y1))

    (gfx:translate (dx dy)
     (backend/translate! backend dx dy))

    ;; Clipping
    (gfx:set-clip-rect (x y w h)
     (backend/set-clip-rect! backend x y w h))

    (gfx:reset-clip ()
     (backend/reset-clip! backend))

    ;; Style
    (gfx:set-pen-color (col)
     (backend/set-pen-color! backend col))

    (gfx:set-fill-color (col)
     (backend/set-fill-color! backend col))

    (gfx:set-line-width (w)
     (backend/set-line-width! backend w))

    (gfx:set-dash (dashes offset)
     (backend/set-dash! backend dashes offset))

    (gfx:set-font (family size slant weight)
     (backend/set-font! backend family size slant weight))

    ;; Stroked primitives
    (gfx:draw-line (x1 y1 x2 y2)
     (backend/draw-line! backend x1 y1 x2 y2))

    (gfx:draw-polyline (pts)
     (backend/draw-polyline! backend pts))

    (gfx:draw-rect (x y w h)
     (backend/draw-rect! backend x y w h))

    (gfx:draw-circle (cx cy r)
     (backend/draw-circle! backend cx cy r))

    (gfx:draw-polygon (pts)
     (backend/draw-polygon! backend pts))

    ;; Filled primitives
    (gfx:draw-filled-rect (x y w h)
     (backend/draw-filled-rect! backend x y w h))

    (gfx:draw-filled-circle (cx cy r)
     (backend/draw-filled-circle! backend cx cy r))

    (gfx:draw-filled-polygon (pts)
     (backend/draw-filled-polygon! backend pts))

    ;; General path
    (gfx:draw-path (cmds filled?)
     (backend/draw-path! backend cmds filled?))

    ;; Text
    (gfx:draw-text (x y text halign valign)
     (backend/draw-text! backend x y text halign valign))

    ;; Group: push state, render body, pop state
    ;; This is the only recursive case.
    (gfx:group (body)
               (begin
                 (backend/push-state! backend)
                 (for-each (lambda (i) (eval-insn! i backend)) body)
                 (backend/pop-state! backend)))
    ))

;;; ================================================================
;;; Top-level render entry points
;;; ================================================================

;;; Render accumulated instructions into an already open backend.
;;; Does not call backend/open! or backend/close!.
;;; Use this when the backend lifecycle is managed externally,
;;; e.g. when rendering multiple VGEs to the same surface in sequence.
(define (vge-render-to! vge backend)
  (for-each
    (lambda (insn) (eval-insn! insn backend))
    (vge-instructions vge)))

;;; Open the backend, render all accumulated instructions, then close.
;;; This is the normal top-level entry point.
(define (vge-render! vge backend)
  (backend/open! backend)
  (vge-render-to! vge backend)
  (backend/close! backend))

;;; ================================================================
;;; Utility: drawer->instructions
;;; ================================================================
;;;
;;; Runs a drawer (a procedure of type (vge -> void)) against a fresh
;;; VGE and returns the resulting instruction list.  Useful for:
;;; testing, serialization, and bounding-box analysis.

(define (drawer->instructions drawer-proc)
  (let ((vge (make-vge)))
    (drawer-proc vge)
    (vge-instructions vge)))

;;; ================================================================
;;; Peephole optimizer
;;; ================================================================
;;;
;;; vge-optimize takes an instruction list and returns a new list
;;; with two transformations applied:
;;;
;;; 1. Style coalescing (within a flat scope):
;;;    Adjacent style-setting instructions of the same variant where
;;;    no draw call appears between them are merged: only the last
;;;    one is kept.  E.g.:
;;;      (gfx:set-pen-color A) (gfx:set-pen-color B) (gfx:draw-line ...)
;;;    -> (gfx:set-pen-color B) (gfx:draw-line ...)
;;;
;;; 2. Empty-group elimination:
;;;    (gfx:group '()) is a no-op and is dropped.
;;;
;;; This function does not mutate the original list.
;;; Call (vge-emit-all! vge (vge-optimize (vge-instructions vge)))
;;; to apply it to a VGE.

(define (vge-optimize insns)
  ;; Pass 1: recurse into groups, drop empty groups.
  (define (opt-pass1 insns)
    (filter-map
      (lambda (i)
        (cases gfx-insn i
          (gfx:group (body)
           (let ((body* (opt-pass1 body)))
             (if (null? body*)
                 #f                      ;; drop empty groups
                 (gfx:group body*))))
          (else i)))
      insns))

  ;; Pass 2: style coalescing within a flat (non-group) scope.
  ;; We scan forward; for each style insn, if the same variant
  ;; appears later before any draw call, drop this one.
  ;;
  ;; Implementation: two-pass over the flat list.
  ;;   First, tag each index with the "last index of same variant
  ;;   before next draw".  Then filter out non-last duplicates.
  (define (style-variant i)
    ;; Returns a symbol identifying the variant of a style instruction,
    ;; or #f if it is not a style instruction.
    (cases gfx-insn i
      (gfx:set-pen-color   (_)       'set-pen-color)
      (gfx:set-fill-color  (_)       'set-fill-color)
      (gfx:set-line-width  (_)       'set-line-width)
      (gfx:set-dash        (_ _)     'set-dash)
      (gfx:set-font        (_ _ _ _) 'set-font)
      (gfx:set-viewport    (_ _ _ _) 'set-viewport)
      (gfx:translate       (_ _)     'translate)
      (else #f)))

  (define (draw-insn? i)
    (cases gfx-insn i
      (gfx:draw-line       (_ _ _ _)   #t)
      (gfx:draw-polyline   (_)         #t)
      (gfx:draw-rect       (_ _ _ _)   #t)
      (gfx:draw-circle     (_ _ _)     #t)
      (gfx:draw-polygon    (_)         #t)
      (gfx:draw-filled-rect    (_ _ _ _) #t)
      (gfx:draw-filled-circle  (_ _ _)   #t)
      (gfx:draw-filled-polygon (_)       #t)
      (gfx:draw-path       (_ _)       #t)
      (gfx:draw-text       (_ _ _ _ _) #t)
      (gfx:group           (_)         #t)  ;; groups may draw; treat as draw
      (else #f)))

  (define (opt-pass2 insns)
    ;; Build a vector for O(1) index access.
    (let* ((v   (list->vector insns))
           (n   (vector-length v))
           ;; drop?(i) = #t if instruction i is a style insn that
           ;; is superseded by the same variant before the next draw.
           (drop? (make-vector n #f)))
      ;; For each style insn at index i, scan forward to find:
      ;;   (a) another style insn of the same variant (supersedes i)
      ;;   (b) a draw insn (terminates the lookahead)
      ;; If (a) is found before (b), mark i for dropping.
      (let loop-outer ((i 0))
        (when (< i n)
          (let ((sv (style-variant (vector-ref v i))))
            (when sv
              ;; Look ahead
              (let look ((j (+ i 1)))
                (when (< j n)
                  (cond
                    ;; Same variant found - i is superseded
                    ((eq? sv (style-variant (vector-ref v j)))
                     (vector-set! drop? i #t))
                    ;; Draw found - stop lookahead
                    ((draw-insn? (vector-ref v j))
                     (void))
                    (else (look (+ j 1)))))))
            (loop-outer (+ i 1)))))
      ;; Collect surviving instructions
      (let loop ((i 0) (acc '()))
        (if (= i n)
            (reverse acc)
            (loop (+ i 1)
                  (if (vector-ref drop? i)
                      acc
                      (cons (vector-ref v i) acc)))))))

  ;; Apply both passes
  (opt-pass2 (opt-pass1 insns)))

(define (print-gfx-insn insn out)
  (pp
   (cases gfx-insn insn
          (gfx:set-viewport (x0 y0 x1 y1)
           `(set-viewport ,x0 ,y0 ,x1 ,y1))
          (gfx:translate (dx dy)
           `(translate ,dx ,dy))
          (gfx:set-clip-rect (x y w h)
           `(set-clip-rect ,x ,y ,w ,h))
          (gfx:reset-clip ()
           `(reset-clip))
          (gfx:set-pen-color (col)
           `(set-pen-color ,col))
          (gfx:set-fill-color (col)
           `(set-fill-color ,col))
          (gfx:set-line-width (w)
           `(set-line-width ,w))
          (gfx:set-dash (d o)
           `(set-dash ,d ,o))
          (gfx:set-font (f s sl w)
           `(set-font ,f ,s ,sl ,w))
          (gfx:draw-line (x1 y1 x2 y2)
           `(draw-line ,x1 ,y1 ,x2 ,y2))
          (gfx:draw-polyline (pts)
           `(draw-polyline ,(length pts) ,pts))
          (gfx:draw-rect (x y w h)
           `(draw-rect x: ,x y: ,y w: ,w h: ,h))
          (gfx:draw-circle (cx cy r)
           `(draw-circle ,cx ,cy ,r))
          (gfx:draw-polygon (pts)
           `(draw-polygon ,(length pts) ,pts))
          (gfx:draw-filled-rect (x y w h)
           `(draw-filled-rect x: ,x y: ,y w: ,w h: ,h))
          (gfx:draw-filled-circle (cx cy r)
           `(draw-filled-circle ,cx ,cy ,r))
          (gfx:draw-filled-polygon (pts)
           `(draw-filled-polygon ,(length pts) ,pts))
          (gfx:draw-path (cmds filled?)
           `(draw-path ,(length cmds) ,cmds filled: ,filled?))
          (gfx:draw-text (x y text ha va)
           `(draw-text ,x ,y ,text ha: ,ha va: ,va))
          (gfx:group (insns)
           `(group ,(length insns) ,insns))
          )
   out))

(set-record-printer! gfx-insn print-gfx-insn)

) ;; end module gg-vge
