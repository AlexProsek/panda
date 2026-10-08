# Numeric types

PaNDA provides numeric records with overloaded arithmetic operators in three
units. They differ in how their precision and storage are determined.

## `panda.Nums`: hardware floating-point complex types

This unit defines complex numbers built from Delphi's hardware floating-point
types:

- `TCmplx64` stores real and imaginary `Single` components.
- `TCmplx128` stores real and imaginary `Double` components.

Their component precision and range are those of `Single` and `Double`.

## `panda.NumsMP`: fixed-size, generic multi-precision types

This unit defines `TMPInt<T>`, `TMPUInt<T>`, and `TMPReal<T>`. The generic
argument `T` is a storage type; the numeric width is fixed for each closed
generic type and is derived from `SizeOf(T)`. For the integer types, the
storage type must be at least 64 bits and meet the limb-size alignment
requirements checked by the unit. Invalid storage types raise
`EArgumentException` when the generic type is initialized.

- `TMPInt<T>` is a signed fixed-width integer using two's-complement
  representation. It provides arithmetic, shifts, bitwise operators,
  comparisons, and integer square root.
- `TMPUInt<T>` is an unsigned fixed-width integer with arithmetic, shifts,
  bitwise operators, comparisons, and integer square root.
- `TMPReal<T>` stores a binary significand in a fixed-size field and an
  exponent. It currently provides initialization from `Double`, conversion
  back to `Double`, and access to its sign, exponent, and 64-bit limbs; it
  does not expose general arithmetic operators.

These are fixed-size values: operations do not increase their width to retain
an oversized result. `TMPInt<T>` and `TMPUInt<T>` are useful when a known,
compile-time storage width is needed. The supported width depends on the
platform limb configuration and the size of `T`.

## `panda.NumsQP`: optimized fixed-precision types

This unit provides fixed-width specializations for common multi-precision
sizes. They are a special case of the fixed-size approach in `panda.NumsMP`:
the widths are declared directly, and common limb arithmetic uses specialized
code with unrolled operations instead of relying on a generic loop over an
arbitrary number of limbs.

- `TInt128` and `TUInt128` are signed and unsigned 128-bit integers.
- `TReal128` is a 128-bit binary floating-point value with a 15-bit exponent
  and 112 stored fraction bits, corresponding to the IEEE 754 binary128
  format.
- `TCmplx256` is a complex value with two `TReal128` components.
- `TUInt256` is an unsigned 256-bit helper type with a limited set of
  arithmetic and shift operations, also declared by this unit.

`TReal128` and `TCmplx256` support arithmetic operators; `TReal128` also
provides reciprocal and square-root operations. Converting `TReal128` to
`Double` loses precision and can raise `ERangeError` when its exponent is
outside the `Double` range. The integer and floating-point widths remain fixed
for all operations.

## `panda.NumsAP`: dynamically sized integers and selectable precision

This unit provides values whose storage is backed by dynamically sized limb
arrays:

- `TInteger` is a signed arbitrary-precision integer. Its arithmetic can grow
  the number of limbs as needed. It supports parsing and formatting,
  comparisons, bitwise operations, division with remainder, and integer
  helpers such as `GCD` and `Power`.
- `TRational` represents a rational value using an integer numerator and
  denominator. Its arithmetic retains a rational representation; conversion
  to `Double` is explicit.
- `TReal` is a binary floating-point value with a variable-length significand
  and an exponent. `Init` accepts a digit count to select working precision;
  if omitted, the unit chooses a default based on the source value. Precision
  can also be changed with `SetPrecision` or `SetBinPrecision`. Conversion
  through `AsDouble` is limited by the `Double` range and precision.
- `TCmplx` stores real and imaginary components as `TReal` values.

The precision of `TReal` is finite for any given value, but its significand
length is not fixed by the type declaration. `TInteger` is not restricted to a
machine integer width, subject to available memory and implementation limits.

## Choosing a representation

Use `TCmplx64` or `TCmplx128` for hardware floating-point complex arithmetic.
Use `TInt128`, `TUInt128`, or `TReal128` for the optimized fixed-width types
provided by `panda.NumsQP`. Use `TMPInt<T>` or `TMPUInt<T>` when another fixed
integer width and generic storage are useful. Use `TInteger` for integers that
may exceed machine widths, `TRational` to preserve rational arithmetic, and
`TReal` when the required binary precision varies between calculations.
`TMPReal<T>` is a fixed-storage extended-significand representation with
conversion support, rather than a full floating-point arithmetic type.
