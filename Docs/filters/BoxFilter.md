# Box filters

`panda.Filters.BoxFilter` provides normalized moving-average box filters for
`Single` data, plus moving-sum and moving-average helpers and a 2D integral
image operation.

## One-dimensional box filter

`TBoxFilter1DF32` calculates the mean over a centered window. `Radius` is the
number of samples on each side of the current sample, so an interior window
contains `2 * Radius + 1` values. At either end, the window is clipped to the
available input and the mean is divided by the number of available values.
The radius defaults to 2; nonpositive assignments are ignored by the inherited
`TFilter1D` setter.

`Execute(aSrc, aDst, aCount)` reads and writes `aCount` contiguous `Single`
values through the `PByte` arguments. The caller owns both buffers and must
provide space for `aCount` values in each.

## Two-dimensional box filter

`TBoxFilter2DF32` calculates a rectangular local mean. `HRadius` and
`VRadius` control the horizontal and vertical extent independently and both
default to 1, inherited from `TFilter2D`. At image borders, the filter uses
only the pixels that lie inside the image and divides by that smaller window
area.

`Execute(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH)` takes source and
destination row steps in bytes, followed by width and height in elements.
Rows may include padding as indicated by their steps. Source and destination
buffers remain caller-owned. The filter owns its working row filter and
releases it when destroyed.

## Moving sum and average helpers

`MovingSum_F32` and `MovingAvg_F32` process a contiguous `Single` sequence
with a forward window of `aKerSz` elements. Each produces
`aCount - aKerSz + 1` values; the average helper divides each sum by the
window size. `aKerSz` must not exceed `aCount` (checked by an assertion).
Both routines write to caller-provided storage and return no separate result.

## Integral image

`Integral2D` computes a 2D summed-area table for a `Single` `INDArray`. If
`aDst` is unassigned, it allocates an output with the source shape. If an
output is supplied, its shape must match the source or an `ENDAShapeError` is
raised. The implementation supports 2D arrays; higher-dimensional inputs do
not have an implemented result path. The source is not owned by this routine;
the output is returned through the `var` parameter.

`TBoxFilter1D` and `TBoxFilter2D` and their generic forms are abstract base
classes for adding box-filter element types. `TBoxFilter1DF32` and
`TBoxFilter2DF32` are the concrete implementations provided by this unit.
