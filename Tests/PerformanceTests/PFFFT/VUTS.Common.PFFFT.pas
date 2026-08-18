unit VUTS.Common.PFFFT;

interface

const
  pffft_lib = 'pffft.dll';

type
  PFFFT_Setup = Pointer;
  pffft_direction_t = (PFFFT_FORWARD = 0, PFFFT_BACKWARD = 1);
  pffft_transform_t = (PFFFT_REAL = 0, PFFFT_COMPLEX = 1);

function pffft_new_setup(N: Integer; transform: pffft_transform_t): PFFFT_Setup;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

procedure pffft_destroy_setup(setup: PFFFT_Setup);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

procedure pffft_transform(setup: PFFFT_Setup; const input, output, work: PSingle;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

procedure pffft_transform_ordered(setup: PFFFT_Setup; const input, output, work: PSingle;
  direction: pffft_direction_t);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

function pffft_is_valid_size(N: Integer; cplx: pffft_transform_t): Integer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

function pffft_aligned_malloc(nb_bytes: NativeUInt): Pointer;
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

procedure pffft_aligned_free(p: Pointer);
  cdecl; external pffft_lib{$IFDEF DELAYEDLOADLIB} delayed{$ENDIF};

implementation

end.
