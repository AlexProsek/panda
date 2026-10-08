# Total-variation filters

`panda.Filters.TVFilter` provides 1D and 2D `Single` total-variation
denoising filters and the `TotalVariationFilter` convenience routine. The
filters minimize a total-variation regularized difference from the input.

## Common settings

`Lambda` controls the data-fidelity term and defaults to 1. `IterationCount`
defaults to 100. Assignments that are not positive are ignored. `StepMonitor`
is an optional method callback that receives the objective value during
iteration and returns whether iteration should continue. `Assign` copies
these settings and the callback from another filter. Filter instances own
their work buffers and release them when destroyed.

## One-dimensional filter

`TTVFilter1DF32.Init(aCount)` initializes a filter for a positive number of
elements. `Execute(aSrc, aDst)` reads and writes 1D `INDArray<Single>` values.
Both arrays must have the initialized length and one dimension. It returns
`True` when initialized and the array shapes are accepted, and `False`
otherwise. Noncontiguous input and output arrays are supported through
internal contiguous buffers.

## Two-dimensional filter

`TTVFilter2DF32.Init(aW, aH)` initializes work buffers for the specified
positive dimensions. `Execute(aSrc, aDst)` requires 2D `INDArray<Single>`
arrays with shape `[aH, aW]`; it returns `False` if the filter is uninitialized
or either shape is invalid, and `True` after writing the result. The source
data is copied into an internal buffer as contiguous elements. The destination
is filled from the computed result.

## Convenience routine

`TotalVariationFilter(aSrc, aDst, aLambda, aIterCount)` creates and runs the
1D or 2D filter selected by `aSrc.NDim`. Its defaults are `aLambda = 1.0` and
`aIterCount = 100`. A source with any other number of dimensions raises
`ENotImplemented`. The destination must already be assigned with the matching
shape. The routine does not return the Boolean result of the filter's
`Execute` method. The current implementation reads `aSrc.Shape[1]` in both
dimension branches. For a 1D source this is outside the shape array's valid
index range; for a 2D source it initializes both dimensions from the column
count, so non-square inputs do not pass the filter's shape check. Use
`TTVFilter1DF32` or `TTVFilter2DF32` directly to avoid these convenience
routine limitations. The routine frees its temporary filter when the call
completes.

`TTVFilter`, `TTVFilter1D`, `TTVFilter1D<T>`, `TTVFilter2D`, and
`TTVFilter2D<T>` are abstract or generic extension points. The provided
concrete implementations are `TTVFilter1DF32` and `TTVFilter2DF32`.
