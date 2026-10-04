# Order-statistics filters

`panda.Filters.OrderStatFilters` provides one-dimensional and two-dimensional
minimum, maximum, median, and local-equalization filters. The filters operate
on caller-provided buffers; `Execute` does not allocate or own the source or
destination data. For 2D calls, row steps are byte distances between rows and
`aW`/`aH` are element counts, not byte counts.

## Minimum and maximum filters

`TMinFilter1DUI8`, `TMaxFilter1DUI8`, `TMinFilter1DF64`, and
`TMaxFilter1DF64` apply a centered one-dimensional min/max window. The radius
is the number of samples on either side of the current sample, so the full
window has `2 * Radius + 1` samples when it fits. At either end, the window is
shortened to the available input samples. The result length is `aCount`.

`TFilter1D.Radius` defaults to 2. Assignments less than or equal to zero are
ignored. The min/max filters inherit `UseHGWMethod` from `TMFilter1D`; this
selects the Van Herk/Gil-Werman path where supported. It defaults to `True`,
although the byte specialization may select another path in assembly builds.

```delphi
var
  Filter: TMinFilter1DUI8;
  Source, Dest: TArray<Byte>;
begin
  Source := TArray<Byte>.Create(5, 4, 3, 2, 5);
  SetLength(Dest, Length(Source));
  Filter := TMinFilter1DUI8.Create;
  try
    Filter.Radius := 1;
    Filter.Execute(@Source[0], @Dest[0], Length(Source));
  finally
    Filter.Free;
  end;
end;
```

`TBoxMinFilter2DUI8` and `TBoxMaxFilter2DUI8` apply separable rectangular
windows. `HRadius` and `VRadius` independently set the horizontal and vertical
radius; both default to 2. The window is clipped at image edges, so only
available pixels contribute. `UseHGWMethod` selects the algorithm used for
the 1D passes and defaults to the underlying 1D filter's setting. `Parallelize`
is inherited from `TGMFilter2D`; it defaults to `False`.

For a 2D call, pass pointers to the first element, source and destination row
steps in bytes, then width and height in elements. The destination must have
space for `aW` elements on each of `aH` rows. These calls preserve row padding
by using the supplied destination step.

`TMinFilter2DUI8` and `TMaxFilter2DUI8` provide min/max filters with an
arbitrary 2D byte mask. Set `Kernel` to a 2D `INDArray<Byte>`; nonzero entries
select samples and zero entries are ignored. The mask dimensions determine
its centered footprint. At borders, missing samples are treated as 255 for a
minimum filter and 0 for a maximum filter, the neutral byte values. `Kernel`
must be assigned before executing the filter. `Assign` copies the kernel and
its execution settings from another filter. `Parallelize` defaults to
`False`.

`TMinDCFilter2DUI8` and `TMaxDCFilter2DUI8` offer rectangular, diamond, and
circle footprints through `KernelType` (`ktRect`, `ktDiamond`, `ktCircle`).
The default is `ktRect`; `Radius` defaults to 1 and nonpositive assignments
are ignored. Rectangular mode uses the box filter directly. Diamond and
circle modes combine a rectangular component with a generated mask. The
`Parallelize` setting is propagated to those component filters.

## Median filters

`TMedianFilter1DUI8` and `TMedianFilter1DF64` use the same centered radius
convention and clipped edge windows as the 1D min/max filters. Radius defaults
to 2. The byte filter returns the middle ordered byte (the upper middle when
the clipped window has an even number of elements). The `Double` filter
returns the middle value for odd window sizes and the average of the two
middle values for even sizes.

`TMedianFilter2DUI8` applies a rectangular window with independently
configurable `HRadius` and `VRadius`; both default to 1. The window is clipped
at image edges and its median is returned for each output pixel. Its byte
median uses the upper middle value for an even-sized clipped window.

## Local equalization

`TLocEqFilter2DUI8` computes a local cumulative-distribution value for each
pixel. For that pixel's clipped rectangular neighborhood, it counts samples
whose value is less than or equal to the pixel value, then writes
`Round(255 * count / neighborhood size)`. This maps the local rank to the byte
range. `HRadius` and `VRadius` default to 1 and define the neighborhood extent
on each axis.

## Shared API and constraints

`TFilter1D.Execute` receives source and destination pointers and an element
count. `TFilter2D.Execute` receives source and destination pointers, byte row
steps, width, and height. Filter instances retain their configured radii,
kernel, and algorithm settings between calls; temporary working buffers are
managed by the filter implementation and released with the instance.

The generic and abstract classes (`TFilter1D`, `TMFilter1D`, `TMFilter1D<T>`,
`TGMFilter2D`, `TBoxMFilter2D`, `TBoxMFilter2D<T>`, `TMFilter2D`,
`TMFilter2D<T>`, `TDCMFilter2D`, `TDCMFilter2D<T>`, and
`THistFilter2DUI8`) are extension points for implementing additional filter
types. `TMedianTrackerUI8` is the histogram-backed byte median/CDF state used
by the median and local-equalization implementations; callers normally use
the filter classes rather than manipulating it directly.

The input and output regions must be valid for the configured dimensions and
their respective row steps. The filters do not validate buffer sizes or row
steps. 1D buffers are contiguous sequences of the element type; `F64` filters
therefore expect `Double` storage even though the API uses `PByte` pointers.
