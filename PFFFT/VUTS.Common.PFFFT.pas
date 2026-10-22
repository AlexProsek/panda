unit VUTS.Common.PFFFT;

{
  IMPORTANT - buffer alignment:
    All buffers (input/output/work) MUST be 16-byte aligned (SIMD requirement).
    Use pffft_aligned_malloc / pffft_aligned_free, NOT plain GetMem/FreeMem.
}

interface

const
  pffft_lib = 'pffft.dll';

type
  PFFFT_Setup = Pointer;
  PFFFTD_Setup = Pointer;

  /// Transform direction: forward (time -> frequency) or backward (inverse).
  pffft_direction_t = (PFFFT_FORWARD = 0, PFFFT_BACKWARD = 1);

  /// Transform type: real-valued or complex-valued input.
  pffft_transform_t = (PFFFT_REAL = 0, PFFFT_COMPLEX = 1);

/// Creates an FFT setup for size N and the given transform type (real/complex).
/// Holds precomputed twiddle factors; reusable and thread-safe (read-only).
/// Returns nil if N is not a supported size. Free with pffft_destroy_setup.
function pffft_new_setup(N: Integer; transform: pffft_transform_t): PFFFT_Setup;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Frees an FFT setup created by pffft_new_setup.
procedure pffft_destroy_setup(setup: PFFFT_Setup);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Performs an FFT (forward or backward) using the given setup.
/// Buffers must be 16-byte aligned (use pffft_aligned_malloc); work may be nil.
/// Output is in internal order - use pffft_transform_ordered for canonical order.
procedure pffft_transform(setup: PFFFT_Setup; const input, output, work: PSingle;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Same as pffft_transform but output is in canonical order (slightly slower).
/// Buffers must be 16-byte aligned (use pffft_aligned_malloc); work may be nil.
procedure pffft_transform_ordered(setup: PFFFT_Setup; const input, output, work: PSingle;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Returns non-zero if N is a supported transform size for the given type.
/// Valid sizes are of the form 2^a * 3^b * 5^c (with a minimum factor requirement).
function pffft_is_valid_size(N: Integer; cplx: pffft_transform_t): Integer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Allocates nb_bytes of 16-byte aligned memory (required for FFT buffers).
/// Free with pffft_aligned_free.
function pffft_aligned_malloc(nb_bytes: NativeUInt): Pointer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Frees memory allocated by pffft_aligned_malloc.
procedure pffft_aligned_free(p: Pointer);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Creates a double-precision FFT setup for size N.
function pffftd_new_setup(N: Integer; transform: pffft_transform_t): PFFFTD_Setup;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Frees a double-precision FFT setup.
procedure pffftd_destroy_setup(setup: PFFFTD_Setup);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Double-precision FFT (internal order). Buffers must be aligned; work may be nil.
procedure pffftd_transform(setup: PFFFTD_Setup; const input, output, work: PDouble;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Double-precision FFT in canonical order (slightly slower).
procedure pffftd_transform_ordered(setup: PFFFTD_Setup; const input, output, work: PDouble;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Returns non-zero if N is a valid double-precision transform size.
function pffftd_is_valid_size(N: Integer; cplx: pffft_transform_t): Integer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Allocates aligned memory for double-precision buffers.
function pffftd_aligned_malloc(nb_bytes: NativeUInt): Pointer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

/// Frees memory from pffftd_aligned_malloc.
procedure pffftd_aligned_free(p: Pointer);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

implementation

end.
