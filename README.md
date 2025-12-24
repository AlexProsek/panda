# PaNDA - Pascal N-Dimensional Arrays

<p align="center">
  <a href="https://github.com/AlexProsek/panda">
    <img src="https://img.shields.io/badge/status-experimental-orange" alt="Status">
  </a>
  <a href="https://github.com/AlexProsek/panda">
    <img src="https://img.shields.io/badge/tested%20with-Delphi%2012-blue" alt="Delphi Version">
  </a>
  <a href="https://github.com/AlexProsek/panda">
    <img src="https://img.shields.io/badge/OpenBLAS-optional%20acceleration-green" alt="OpenBLAS">
  </a>
  <a href="https://github.com/AlexProsek/panda/blob/main/LICENSE">
    <img src="https://img.shields.io/github/license/AlexProsek/panda" alt="License">
  </a>
  <a href="https://github.com/AlexProsek/panda/stargazers">
    <img src="https://img.shields.io/github/stars/AlexProsek/panda?style=flat" alt="GitHub stars">
  </a>
  <a href="https://github.com/AlexProsek/panda/releases">
    <img src="https://img.shields.io/github/v/release/AlexProsek/panda?label=release" alt="Release">
  </a>
</p>

`PaNDA` is a small library that provides classes for working with N-dimensional arrays in Pascal (Delphi).
It introduces an `INDArray` interface that supports slicing syntax similar to Python and Mathematica.
Some basic examples are shown below:

| Python       | PaNDA |
|--------------|-------|
| `a[0]`       | `a[[NDI(0)]]` |
| `a[1:6:2]`   | `a[[NDISpan(1,6,2)]]` |
| `a[::2]`     | `a[[NDISpan(0, -1, 2)]]` or `a[[NDIAll(2)]]` |
| `a[::2,::2]` | `a[[NDAll(2), NDIAll(2)]]` |
| `a[[0,2,2]]` | `a[[NDISet([0,2,2])]]` or `a[[NDI([0,2,2])]]` |
| `a[::,1]`    | `a[[NDIAll, NDI(1)]]` |

### Delphi Compatibility ###

PaNDA has been tested with **Delphi 12** (Athens). 
Other Delphi versions may work, but have not been fully verified.

## Tensor Types ##

The library also provides tensor types, implemented as records, which wrap ND-arrays and support basic arithmetic operations through operator overloading.

Example:

```pascal
var a, b, c: TTensorF32;
begin
  a := TNDAUt.AsArray([1, 2, 3]);
  b := TNDAUt.AsArray([3, 2, 1]);
  c := (a + b)/2;
end;
```

The library includes common tensor functions such as `Dot`, `Inner`, and `Outer` products.
Where available, these functions can use optimized routines from **OpenBLAS** (described below).
PaNDA also includes several optimized assembly implementations that require SSE2/SSE3 instruction sets
(assembly can be disabled using the `NoASM` compiler directive).

## Optional OpenBLAS Acceleration

Using OpenBLAS is optional. To enable it, the following conditions must be met:

1. **Define the `PANDA_BLAS` environment variable**  
  Point it to the directory containing the OpenBLAS binaries.  
  The directory should include platform-specific subfolders:
  ```
    <your-openblas-path>\
      Win32\libopenblas.dll
      Win64\libopenblas.dll
  ```

2. **Compile the library with the `BLAS` compiler directive**  
  Add `BLAS` to your project's conditional defines.

### OpenBLAS/CBLAS Interface ###

PaNDA uses the Delphi port of the CBLAS interface provided by [Eric Grange’s LibCBLAS](https://github.com/EricGrange/LibCBLAS) when OpenBLAS acceleration is enabled.

## PaNDA4Py ##

The library includes the `TPyNDArray` type, which wraps a PaNDA array and exposes the **NumPy array interface protocol**.
This allows sharing arrays between Python and Delphi, using the `Python4Delphi` framework.