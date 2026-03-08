# ggplot

A Grammar of Graphics plotting library for [CHICKEN Scheme](https://www.call-cc.org/), inspired by [ggplot2](https://ggplot2.tidyverse.org/). Plots are described as composable declarative S-expressions that are translated into vector graphics via [GNU libplot](https://www.gnu.org/software/plotutils/).

## Features

- Declarative, layered plot specification with `ggplot` + `layer`
- Automatic scale training from data - no manual range computation needed
- Continuous, discrete, logarithmic, and color scales
- Axes with configurable tick marks and labels
- Statistical transformations: histograms, density estimates, boxplots, violin plots, summary statistics
- Faceted layouts (small multiples)
- Annotation layers: text, rectangles, segments, arrows
- Multiple output backends: PNG, SVG, PostScript, X11
- Seven composable modules usable independently or together

## Installation

The library is packaged as a CHICKEN 5 egg. Install with:

```sh
chicken-install ggplot
```

### Dependencies

The following eggs must be available:

- [`plot`](https://wiki.call-cc.org/eggref/5/plot) - GNU libplot wrapper
- [`matchable`](https://wiki.call-cc.org/eggref/5/matchable) - pattern matching
- [`datatype`](https://wiki.call-cc.org/eggref/5/datatype) - algebraic data types
- [`statistics`](https://wiki.call-cc.org/eggref/5/statistics) - statistical functions
- [`yasos`](https://wiki.call-cc.org/eggref/5/yasos) - object system (used internally)

## Quick Start

```scheme
(import gg-plot gg-primitives gg-scales gg-aes gg-geom)

(define data
  '((x . (1 2 3 4 5))
    (y . (2.1 3.8 3.2 5.0 4.7))))

(define p
  (ggplot data (aes #:x 'x #:y 'y)
    (layer 'point #:size 6 #:color "steelblue")
    (layer 'line  #:color "steelblue" #:width 2)
    (labs  #:title "Simple scatter plot"
           #:x "X" #:y "Y")
    (theme-minimal)))

(let ((ctx (make-png-plotter "plot.png" 800 600)))
  (render-plot p ctx)
  (delete-plotter (plotter-context-plotter ctx)))
```

## Modules

The library is split into seven modules that can be used independently
or together.

| Module | Purpose |
|---|---|
| `gg-primitives` | Low-level drawing combinators wrapping GNU libplot |
| `gg-scales` | Data-to-visual mappings (domain training, break computation) |
| `gg-data` | Columnar data frame utilities |
| `gg-aes` | Aesthetic mappings between data columns and visual channels |
| `gg-geom` | Geometric objects (point, line, bar, …) |
| `gg-guides` | Axes and legends |
| `gg-plot` | Top-level declarative API |

### Data format

Data is passed as an association list of named columns:

```scheme
'((time    . (0 10 20 30 40 50))
  (value   . (1.2 1.8 2.3 1.9 2.5 2.1))
  (group   . ("A" "A" "A" "B" "B" "B")))
```

## Plot API (`gg-plot`)

### `ggplot`

```scheme
(ggplot data aesthetic layer ... scale ... theme ... label-spec)
```

Creates a complete plot specification. All arguments after the aesthetic mapping are gathered as plot elements.

### `layer`

```scheme
(layer geom-name
       #:mapping aes      ; optional per-layer aesthetic override
       #:data    data     ; optional per-layer data override
       #:stat    stat     ; optional statistical transformation
       param ...)         ; geometry-specific keyword parameters
```

### Layer constructors

Convenience constructors matching common geometry names:

```
layer-point   layer-line      layer-path      layer-area
layer-bar     layer-rect      layer-text      layer-segment
layer-hline   layer-vline     layer-eventplot
layer-histogram  layer-density  layer-boxplot  layer-violin
layer-errorbar   layer-pointrange  layer-linerange  layer-crossbar
layer-col
```

Annotation layers (placed in data coordinates):

```
layer-annotate-text   layer-annotate-rect
layer-annotate-segment  layer-annotate-arrow
```

### Scales

```scheme
(scale-x-continuous #:name "label" #:limits '(0 . 100) #:trans 'log)
(scale-y-continuous #:name "label")
(scale-color-gradient #:low "white" #:high "blue")
(scale-color-manual #:values '(("A" . "red") ("B" . "blue")))
(scale-fill-gradient #:low "white" #:high "red")
```

### Labels

```scheme
(labs #:title "Title" #:subtitle "Subtitle" #:x "X axis" #:y "Y axis")
```

### Themes

```scheme
(theme-minimal  #:base-size 12.0)
(theme-classic  #:base-size 12.0)
(theme-dark     #:base-size 12.0)
```

### Output backends

```scheme
(make-png-plotter filename width height)   ; PNG raster
(make-svg-plotter filename width height)   ; SVG vector
(make-ps-plotter  filename width height)   ; PostScript
(make-x-plotter   width height)           ; X11 interactive window
```

Render and finalise:

```scheme
(let ((ctx (make-png-plotter "out.png" 800 600)))
  (render-plot plot ctx)
  (delete-plotter (plotter-context-plotter ctx)))
```

## Examples

### Time series with confidence ribbon

```scheme
(define data
  '((t     . (0 10 20 30 40 50 60 70 80 90 100))
    (value . (1.2 1.5 2.3 1.8 1.4 1.9 2.5 2.1 1.7 2.0 2.3))
    (upper . (1.5 1.8 2.6 2.1 1.7 2.2 2.8 2.4 2.0 2.3 2.6))
    (lower . (0.9 1.2 2.0 1.5 1.1 1.6 2.2 1.8 1.4 1.7 2.0))))

(ggplot data (aes #:x 't #:y 'value)
  (layer 'area  #:mapping (aes #:ymin 'lower #:ymax 'upper)
                #:fill "lightblue" #:alpha 0.3)
  (layer 'line  #:color "steelblue" #:width 2)
  (layer 'point #:size 5 #:color "darkblue")
  (scale-x-continuous #:name "Time (s)")
  (scale-y-continuous #:name "Response (mV)")
  (labs #:title "Time Series with Confidence Band")
  (theme-minimal))
```

### Bar chart

```scheme
(define data
  '((category . ("A" "B" "C" "D"))
    (count    . (23 41 17 35))))

(ggplot data (aes #:x 'category #:y 'count)
  (layer 'bar #:fill "steelblue")
  (labs #:title "Category Counts" #:x "Category" #:y "Count")
  (theme-minimal))
```

### Multi-series with color mapping

```scheme
(ggplot data (aes #:x 'time #:y 'value #:color 'condition)
  (layer 'line  #:width 2)
  (layer 'point #:size 4)
  (scale-color-manual
    #:values '(("Control" . "steelblue") ("Treatment" . "firebrick")))
  (labs #:title "Condition Comparison")
  (theme-minimal))
```

### Faceted small multiples

```scheme
(ggplot data (aes #:x 'x #:y 'y)
  (layer 'point #:size 4)
  (facet-wrap 'group #:ncol 2)
  (theme-minimal))
```

### Logarithmic axis

```scheme
(ggplot data (aes #:x 'frequency #:y 'power)
  (layer 'line #:width 2)
  (scale-x-continuous #:trans 'log10 #:name "Frequency (Hz)")
  (scale-y-continuous #:trans 'log10 #:name "Power (dB)")
  (theme-minimal))
```

### Statistical geometries

```scheme
;; Error bars from raw data (computes mean +/- SE automatically)
(ggplot iris (aes #:x 'species #:y 'sepal_length)
  (layer-pointrange #:stat 'summary)
  (labs #:title "Mean Sepal Length ± SE by Species"))

;; Boxplot
(ggplot iris (aes #:x 'species #:y 'petal_length)
  (layer 'boxplot #:fill "lightblue")
  (labs #:title "Petal Length by Species"))
```

## Lower-level usage

The modules below `gg-plot` can be used directly for custom plot pipelines.

### Scales (`gg-scales`)

```scheme
(import gg-scales)

(define s (make-scale-linear))
(scale-train! s '(0 1 2 3 4 5))
(scale-set-range! s '(80 . 720))
(scale-map s 2.5)    ; pixel coordinate
(scale-breaks s 5)   ; list of tick positions
```

### Drawing primitives (`gg-primitives`)

```scheme
(import gg-primitives)

;; Drawers are values; combine composes them
(define fig
  (combine
    (rectangle 0 0 800 600 #:fill-color "white")
    (with-pen-color "steelblue"
      (circle 400 300 50))
    (text 400 560 "Hello" #:size 14.0)))

(render fig (make-png-plotter "fig.png" 800 600))
```

## Serialisation

Plot specifications can be converted to and from S-expressions for
storage, transmission, or code generation:

```scheme
(define sexp (plot->sexp plot))       ; plot → S-expression
(define plot (sexp->plot sexp))       ; S-expression → plot
```

## License

[GPL-3](https://www.gnu.org/licenses/gpl-3.0.html)
