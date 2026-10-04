# Guided filter

`panda.Filters.GuidedFilter` provides `TGuidedFilter2DF32`, a 2D guided
filter for `Single` arrays. It uses a guide image to estimate a local linear
model and applies that model to the input image. The input and guide should
have the same height and width; the output has that same shape.

## Setup and execution

Call `Init(aGuide)` with an `INDArray<Single>` guide before calling
`Execute(pSrc, pDst, aSrcWStep, aDstWStep, aW, aH)`. The generic `Init`
converts the guide to a contiguous array and keeps it for subsequent calls.
`Execute` consumes the source and writes the result to the caller-provided
destination buffer. The filter owns its intermediate arrays and box filter;
the caller retains ownership of the guide, source, and destination arrays.

`HRadius` and `VRadius` set the local rectangular model window and default to
1. `Epsilon` is the positive regularization value added to local guide
variance; it defaults to `1e-2`. Assignments that are not positive are
ignored. The underlying 2D box filter clips its windows at image edges and
normalizes by the number of in-image pixels.

The base class `TGuidedFilter2D` defines the filter contract and arithmetic
hooks. `TGuidedFilter2D<T>` supplies typed array wrapping and guide setup;
`TGuidedFilter2DF32` is the concrete arithmetic implementation in this unit.
Its base `Execute` method accepts raw pointers and byte row steps. The guide
is made contiguous during initialization, and callers should provide a
contiguous source image with tightly packed rows; the source row-step
parameter is not used by the implementation. The destination row step is
used when writing output.
