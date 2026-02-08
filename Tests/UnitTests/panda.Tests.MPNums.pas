unit panda.Tests.MPNums;

interface

uses
    TestFramework
  , System.SysUtils
  , panda.MPNums
  , panda.Tests.NDATestCase
  ;

{$I AsmDefs.inc}

{$if defined(ASMx64)}
  {$define Limb64}
{$else}
  {$define Limb32}
{$endif}

type
  TLowLvlTests = class(TNDATestCase)
  published
    procedure TopBitPos;
    procedure TopBitPosOfArray;
    procedure CountLeadingZeros;
    procedure CountTrailingZeros;

    procedure BitShiftLeft;
    procedure BitShiftLeftInPlace;
    procedure BitShiftLeftInPlace40;
    procedure BitShiftRight;
    procedure BitShiftRightInPlace;
    procedure BitShiftRightInPlace40;
    procedure TestLength;

    procedure LongShift_NoShift;
    procedure LongShift_SHL;
    procedure LongShift_SHR;
  end;

  TInt128Tests = class(TNDATestCase)
  published
    // P - positive number, N - negative number, _(P|N) result
    procedure AddPP;
    procedure SubPP_P;
    procedure SubPP_N;
    procedure NegP;
    procedure NegN;
    procedure MulPP;
    procedure MulNP;
    procedure TestShl;
    procedure TestShr;
    procedure ShortDiv;
    procedure LongDiv;
    procedure LessNN;
    procedure LessNP;
  {$ifopt Q+}
    procedure AddOverflowPP;
    procedure AddOverflowNN;
    procedure SubOverflowNP;
    procedure SubOverflowPN;
    procedure NegOverflow;
    procedure MulOverflow;
  {$endif}
  end;

  TUInt128Tests = class(TNDATestCase)
  published
    procedure Add;
    procedure Sub;
    procedure Mul;
    procedure ShortDiv;
    procedure LongDiv;
    procedure Less;
    procedure Incr;
  {$ifopt Q+}
    procedure AddOverflow;
    procedure SubOverflow;
    procedure MulOverflow;
    procedure IncOverflow;
  {$endif}
  end;

  TReal128Tests = class(TNDATestCase)
  // OF - overflow
  published
    procedure FromDouble;
    procedure FromDoubleSubnormal;
    procedure RealToDbl_DblPrec;
    procedure RealToDbl_SubDblPrec;
    procedure RealToDbl_DblSubnormal;
    procedure RealToDbl_ExpOF;
    procedure RealToDbl_RoundUpWithExpOF;
    procedure AddDblPP;
    procedure Add_SubDblPrec;
    procedure Add_RoundUpWthoutOF;
    procedure Add_TieToEvenWithoutOF;
    procedure Add_RoundUpWithOF;
    procedure Add_TieToEvenWithOF;
    procedure Sub_SameExp;
    procedure MulDbl;
    procedure Reciprocal;
    procedure DivideDbl;
    procedure Sqrt;
  end;

  TCmplx256Tests = class(TNDATestCase)
  published
    procedure AddDbl;
    procedure SubDbl;
  end;

implementation

{$region 'TLowLvlTests'}

procedure TLowLvlTests.TopBitPos;
var A: TLimb;
    I: Integer;
begin
  A := 0;
  I := _TopBitPos(A);
  CheckEquals(0, I, 'TopBit(0)');

  A := $1;
  I := _TopBitPos(A);
  CheckEquals(1, I, 'TopBit(1)');

  A := $1000;
  I := _TopBitPos(A);
  CheckEquals(13, I, 'TopBit($1000)');

  A := $8000;
  I := _TopBitPos(A);
  CheckEquals(16, I, 'TopBit($8000)');

{$ifdef Limb64}
  A := $100000000;
  I := _TopBitPos(A);
  CheckEquals(33, I);

  A := $8000000000000000;
  I := _TopBitPos(A);
  CheckEquals(64, I);
{$endif}
end;

procedure TLowLvlTests.TopBitPosOfArray;
var A: array [0..1] of TLimb;
    I: Integer;
begin
  A[0] := 0;
  A[1] := $80000000;
  I := _TopBitPos(@A[0], 2);
{$ifndef Limb64}
  CheckEquals(64, I);
{$else}
  CheckEquals(96, I);
{$endif}

  A[0] := 0;
  A[1] := 8;
  I := _TopBitPos(@A[0], 2);
{$ifndef Limb64}
  CheckEquals(36, I);
{$else}
  CheckEquals(68, I);
{$endif}

  A[0] := $80000000;
  A[1] := 0;
  I := _TopBitPos(@A[0], 2);
  CheckEquals(32, I);
  I := _TopBitPos(@A[0], 1);
  CheckEquals(32, I);

  A[0] := 0;
  A[1] := 0;
  I := _TopBitPos(@A[0], 2);
  CheckEquals(0, I);
  I := _TopBitPos(@A[0], 1);
  CheckEquals(0, I);
end;

procedure TLowLvlTests.CountLeadingZeros;
var A: TLimb;
    I: Integer;
begin
  A := {$ifdef Limb64}$8000000000000000{$else}$80000000{$endif};
  I := _CountLeadingZeros(A);
  CheckEquals(0, I);

  A := {$ifdef Limb64}$0800000000000000{$else}$08000000{$endif};
  I := _CountLeadingZeros(A);
  CheckEquals(4, I);

  A := 0;
  I := _CountLeadingZeros(A);
  CheckEquals(W_BIT_COUNT, I);
end;

procedure TLowLvlTests.CountTrailingZeros;
var A: TLimb;
    I: Integer;
begin
  A := $80000000;
  I := _CountTrailingZeros(A);
  CheckEquals(31, I);

  A := $00000001;
  I := _CountTrailingZeros(A);
  CheckEquals(0, I);

  A := $00000010;
  I := _CountTrailingZeros(A);
  CheckEquals(4, I);

  A := 0;
  I := _CountTrailingZeros(A);
  CheckEquals(W_BIT_COUNT, I);
end;

procedure TLowLvlTests.BitShiftLeft;
var A: array [0..1] of TLimb;
    B: array [0..2] of TLimb;
    tmp: UInt64 absolute A;
begin
{$ifdef Limb64}
  A[0] := $123456789ABCDEF0;
  A[1] := $FEDCBA9876543210;
{$else}
  tmp := $12345678ABCDEF12;
{$endif}
  B[2] := _shl(@A, @B, 8, 2);
{$ifdef Limb64}
  CheckEquals($3456789ABCDEF000, B[0]);
  CheckEquals($DCBA987654321012, B[1]);
  CheckEquals($00000000000000FE, B[2]);
{$else}
  CheckEquals($CDEF1200, B[0]);
  CheckEquals($345678AB, B[1]);
  CheckEquals($00000012, B[2]);
{$endif}
end;

procedure TLowLvlTests.BitShiftLeftInPlace;
var A: array [0..1] of TLimb;
    tmp: UInt64 absolute A;
    carry: TLimb;
begin
{$ifdef Limb64}
  A[1] := $123456789ABCDEF1;
  A[0] := $23456789ABCDEF12;
{$else}
  tmp := $12345678ABCDEF12;
{$endif}
  carry := _shl(@A, @A, 8, 2);
{$ifdef Limb64}
  CheckEquals($12, carry);
  CheckEquals($3456789ABCDEF123, A[1]);
  CheckEquals($456789ABCDEF1200, A[0]);
{$else}
  CheckEquals($12, carry);
  CheckEquals($CDEF1200, A[0]);
  CheckEquals($345678AB, A[1]);
{$endif}
end;

procedure TLowLvlTests.BitShiftLeftInPlace40;
var B: array [0..2] of TLimb;
    carry: TLimb;
begin
{$ifdef Limb64}
  B[2] := $123456789ABCDEF1;
  B[1] := $23456789ABCDEF12;
  B[0] := $3456789ABCDEF123;
{$else}
  B[2] := $12345678;
  B[1] := $9ABCDEF1;
  B[0] := $23456789;
{$endif}
  carry := _shl(@B[0], @B[1], 8, 2);
{$ifdef Limb64}
  CheckEquals($23, carry);
  CheckEquals($456789ABCDEF1234, B[2]);
  CheckEquals($56789ABCDEF12300, B[1]);
{$else}
  CheckEquals($9A, carry);
  CheckEquals($45678900, B[1]);
  CheckEquals($BCDEF123, B[2]);
{$endif}
end;

procedure TLowLvlTests.BitShiftRight;
var A: array [0..1] of TLimb;
    B: array [0..1] of TLimb;
    tmp: UInt64 absolute A;
    carry: TLimb;
begin
{$ifdef Limb64}
  A[0] := $123456789ABCDEF1;
  A[1] := $1FEDCBA987654321;
{$else}
  tmp := $12345678ABCDEF12;
{$endif}
  carry := _shr(@A, @B, 8, 2);
{$ifdef Limb64}
  CheckEquals($F100000000000000, carry);
  CheckEquals($21123456789ABCDE, B[0]);
  CheckEquals($001FEDCBA9876543, B[1]);
{$else}
  CheckEquals($12000000, carry);
  CheckEquals($78ABCDEF, B[0]);
  CheckEquals($00123456, B[1]);
{$endif}
end;

procedure TLowLvlTests.BitShiftRightInPlace;
var A: array [0..1] of TLimb;
    B: array [0..2] of TLimb;
    tmp: UInt64 absolute A;
    carry: TLimb;
begin
{$ifdef Limb64}
  A[0] := $123456789ABCDEF1;
  A[1] := $1FEDCBA987654321;
{$else}
  tmp := $12345678ABCDEF12;
{$endif}
  carry := _shr(@A, @A, 8, 2);
{$ifdef Limb64}
  CheckEquals($F100000000000000, carry);
  CheckEquals($21123456789ABCDE, A[0]);
  CheckEquals($001FEDCBA9876543, A[1]);
{$else}
  CheckEquals($12000000, carry);
  CheckEquals($78ABCDEF, A[0]);
  CheckEquals($00123456, A[1]);
{$endif}

{$ifdef Limb64}
  B[2] := $123456789ABCDEF1;
  B[1] := $1FEDCBA987654321;
  B[0] := $123456789ABCDEFE;
{$else}
  B[2] := $12345678;
  B[1] := $9ABCDEF1;
  B[0] := $23456789;
{$endif}
  carry := _shr(@B, @B, 8, 3);
{$ifdef Limb64}
  CheckEquals($FE00000000000000, carry);
  CheckEquals($21123456789ABCDE, B[0]);
  CheckEquals($F11FEDCBA9876543, B[1]);
  CheckEquals($00123456789ABCDE, B[2]);
{$else}
  CheckEquals($89000000, carry);
  CheckEquals($F1234567, B[0]);
  CheckEquals($789ABCDE, B[1]);
  CheckEquals($00123456, B[2]);
{$endif}
end;

procedure TLowLvlTests.BitShiftRightInPlace40;
var B: array [0..2] of TLimb;
    carry: TLimb;
begin
{$ifdef Limb64}
  B[2] := $123456789ABCDEF1;
  B[1] := $23456789ABCDEF12;
  B[0] := $3456789ABCDEF123;
{$else}
  B[2] := $12345678;
  B[1] := $9ABCDEF1;
  B[0] := $23456789;
{$endif}
  carry := _shr(@B[1], @B[0], 8, 2);
{$ifdef Limb64}
  CheckEquals($1200000000000000, carry);
  CheckEquals($F123456789ABCDEF, B[0]);
  CheckEquals($00123456789ABCDE, B[1]);
{$else}
  CheckEquals($F1000000, carry);
  CheckEquals($789ABCDE, B[0]);
  CheckEquals($00123456, B[1]);
{$endif}
end;

procedure TLowLvlTests.TestLength;
var A: array [0..3] of TLimb;
    I: Integer;
begin
  A[0] := 0;
  A[1] := 0;
  A[2] := 0;
  I := _Length(@A, 3);
  CheckEquals(0, I);

  A[0] := 1;
  I := _Length(@A, 3);
  CheckEquals(1, I);

  A[1] := 1;
  I := _Length(@A, 3);
  CheckEquals(2, I);

  A[2] := 2;
  I := _Length(@A, 3);
  CheckEquals(3, I);
end;

procedure TLowLvlTests.LongShift_NoShift;
var a: array [0..2] of TLimb;
begin
  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, 0);
  CheckEquals(1, a[0]);
  CheckEquals(2, a[1]);
  CheckEquals(3, a[2]);
end;

procedure TLowLvlTests.LongShift_SHL;
var a: array [0..2] of TLimb;
begin
  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, 1);
  CheckEquals(2, a[0]);
  CheckEquals(4, a[1]);
  CheckEquals(6, a[2]);

  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, cLimbBits);
  CheckEquals(0, a[0]);
  CheckEquals(1, a[1]);
  CheckEquals(2, a[2]);

  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, cLimbBits + 1);
  CheckEquals(0, a[0]);
  CheckEquals(2, a[1]);
  CheckEquals(4, a[2]);
end;

procedure TLowLvlTests.LongShift_SHR;
var a: array [0..2] of TLimb;
begin
  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, -1);
  CheckEquals(0, a[0]);
  CheckEquals(1 or W_HI_BIT, a[1]);
  CheckEquals(1, a[2]);

  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, -cLimbBits);
  CheckEquals(2, a[0]);
  CheckEquals(3, a[1]);
  CheckEquals(0, a[2]);

  a[0] := 1; a[1] := 2; a[2] := 3;
  _ipLongShift(@a, 3, -cLimbBits + 1);
  CheckEquals(4, a[0]);
  CheckEquals(6, a[1]);
  CheckEquals(0, a[2]);
end;

{$endregion}

{$region 'TInt128Tests'}

procedure TInt128Tests.AddPP;
var a, b, c: TInt128;
    i: Int64;
begin
  a := 4;
  b := 12;
  c := a + b;
  i := Int64(c);
  CheckEquals(16, i);

  a := $ffffffff;
  b := $1;
  c := a + b;
  i := Int64(c);
  CheckEquals($100000000, i);

  a.Init(High(UInt64), 0);
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals(1, c.Hi);

  a.Init(High(UInt64), $ffffffff);
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals($100000000, c.Hi);
end;

procedure TInt128Tests.SubPP_P;
var a, b, c: TInt128;
begin
  a := 12;
  b := 4;
  c := a - b;
  CheckEquals(8, Int64(c));

  a := $100000000;
  b := $1;
  c := a - b;
  CheckEquals($ffffffff, c.Lo);
  CheckEquals(0, c.Hi);

  a := $1000000000;
  c := a - b;
  CheckEquals($fffffffff, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, 1);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, $100000000);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals($ffffffff, c.Hi);

  a.Init(0, $1000000000);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals($fffffffff, c.Hi);
end;

procedure TInt128Tests.SubPP_N;
var a, b, c: TInt128;
begin
  a := 4;
  b := 12;
  c := a - b;
  CheckEquals(-8, Int64(c));
  CheckEquals(High(UInt64), c.Hi);
end;

procedure TInt128Tests.NegP;
var a: TInt128;
begin
  a := 21;
  a := -a;
  CheckEquals(-21, Int64(a));
  CheckEquals(High(UInt64), a.Hi);

  a := $100000000;
  a := -a;
  CheckEquals(-$100000000, Int64(a));
  CheckEquals(High(UInt64), a.Hi);

  a.Init(0, 1);
  a := -a;
  CheckEquals(-1, Int64(a.Hi));
end;

procedure TInt128Tests.NegN;
var a: TInt128;
begin
  a := -21;
  a := -a;
  CheckEquals(21, Int64(a));
  CheckEquals(0, a.Hi);

  a := -$100000000;
  a := -a;
  CheckEquals($100000000, Int64(a));
  CheckEquals(0, a.Hi);
end;

procedure TInt128Tests.MulPP;
var a, b, c: TInt128;
begin
  a := $11111111;
  b := $99999999;
  c := a * b;
  CheckEquals($A3D70A3C28F5C29, Int64(c));
  CheckEquals(0, c.Hi);

  a := $1111111111111111;
  b := $9999999999999999;
  c := a * b;

  CheckEquals($8f5c28f5c28f5c29, c.Lo);
  CheckEquals($a3d70a3d70a3d70, c.Hi);
end;

procedure TInt128Tests.MulNP;
var a, b, c: TInt128;
begin
  a := -12;
  b := 2;
  c := a * b;
  CheckEquals(-24, Int64(c));
  CheckEquals(High(UInt64), c.Hi);
end;

procedure TInt128Tests.TestShl;
var a, b: TInt128;
begin
  a := $f;
  b := a shl 8;
  CheckEquals($f00, b.Lo);
  CheckEquals(0, b.Hi);

  b := a shl 32;
  CheckEquals($f00000000, b.Lo);
  CheckEquals(0, b.Hi);

  a := TInt128(1) shl 65;
  CheckEquals(0, a.Lo);
  CheckEquals(2, a.Hi);

  a := -$f;
  b := a shl 8;
  CheckEquals(-$f00, Int64(b.Lo));
  CheckEquals(High(UInt64), b.Hi);

  a := TInt128(1) shl 129;
  CheckEquals(0, a.Lo);
  CheckEquals(0, a.Hi);
end;

procedure TInt128Tests.TestShr;
var a, b: TInt128;
begin
  a := $f0000000000;
  b := a shr 8;
  CheckEquals($f00000000, b.Lo);
  CheckEquals(0, b.Hi);

  b := a shr 32;
  CheckEquals($f00, b.Lo);
  CheckEquals(0, b.Hi);

  b := a shr 33;
  CheckEquals($780, b.Lo);
  CheckEquals(0, b.Hi);

  a := TInt128(1) shl 64;
  b := a shr 63;
  CheckEquals(2, b.Lo);
  CheckEquals(0, b.Hi);
end;

procedure TInt128Tests.ShortDiv;
var a, b, c: TInt128;
begin
  a := $2222222222;
  b := $11;
  c := a div b;
  CheckEquals($2222222222 div $11, UInt64(c));

  a.Init($2222222222222222, $2222222222);
  b := $11;
  c := a div b;
  CheckEquals($0202020202020202, c.Lo);
  CheckEquals($202020202, c.Hi);
end;

procedure TInt128Tests.LongDiv;
var a, b, c: TInt128;
begin
  a.Init($2222222222222222, $22222222);
  b.Init($1111111111111111, $1111);
  c := a div b;
  CheckEquals($20000, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, $7000000000000000);
  b.Init(0, 2);
  c := a div b;
  CheckEquals($3800000000000000, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, 1);
  b.Init(0, 2);
  c := a div b;
  CheckEquals(0, c.Lo);
  CheckEquals(0, c.Hi);
end;

procedure TInt128Tests.LessNN;
var a, b: TInt128;
begin
  a := -2;
  b := -3;
  CheckTrue(b < a);
  CheckFalse(a < b);

  a.Init(0, UInt64(-2));
  b.Init(0, UInt64(-3));
  CheckTrue(b < a);
  CheckFalse(a < b);
end;

procedure TInt128Tests.LessNP;
var a, b: TInt128;
begin
  a := -2;
  b := 3;
  CheckTrue(a < b);
  CheckFalse(b < a);

  a.Init(0, UInt64(-2));
  b.Init(0, 3);
  CheckTrue(a < b);
  CheckFalse(b < a);
end;

{$ifopt Q+}

procedure TInt128Tests.AddOverflowPP;
var a: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(Int64));
  a := a + a;
end;

procedure TInt128Tests.AddOverflowNN;
var a: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(0, cBit64);
  a := a + a;
end;

procedure TInt128Tests.SubOverflowNP;
var a, b: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(0, cBit64);
  b := 10;
  a := a - b;
end;

procedure TInt128Tests.SubOverflowPN;
var a, b: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(UInt64) - cBit64);
  b := -10;
  a := a - b;
end;

procedure TInt128Tests.NegOverflow;
var a: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(0, cBit64);
  a := -a;
end;

procedure TInt128Tests.MulOverflow;
var a: TInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(Int64));
  a := a * a;
end;

{$endif}

{$endregion}

{$region 'TUInt128Tests'}

procedure TUInt128Tests.Add;
var a, b, c: TUInt128;
    i: Int64;
begin
  a := 4;
  b := 12;
  c := a + b;
  i := Int64(c);
  CheckEquals(16, i);

  a := $ffffffff;
  b := $1;
  c := a + b;
  i := Int64(c);
  CheckEquals($100000000, i);

  a.Init(High(UInt64), 0);
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals(1, c.Hi);

  a.Init(High(UInt64), $ffffffff);
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals($100000000, c.Hi);
end;

procedure TUInt128Tests.Sub;
var a, b, c: TUInt128;
begin
  a := 12;
  b := 4;
  c := a - b;
  CheckEquals(8, Int64(c));

  a := $100000000;
  b := $1;
  c := a - b;
  CheckEquals($ffffffff, c.Lo);
  CheckEquals(0, c.Hi);

  a := $1000000000;
  c := a - b;
  CheckEquals($fffffffff, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, 1);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, $100000000);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals($ffffffff, c.Hi);

  a.Init(0, $1000000000);
  c := a - b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals($fffffffff, c.Hi);
end;

procedure TUInt128Tests.Mul;
var a, b, c: TUInt128;
begin
  a := $11111111;
  b := $99999999;
  c := a * b;
  CheckEquals($A3D70A3C28F5C29, Int64(c));
  CheckEquals(0, c.Hi);

  a := $1111111111111111;
  b := $9999999999999999;
  c := a * b;

  CheckEquals($8f5c28f5c28f5c29, c.Lo);
  CheckEquals($a3d70a3d70a3d70, c.Hi);
end;

procedure TUInt128Tests.ShortDiv;
var a, b, c: TUInt128;
begin
  a := $2222222222;
  b := $11;
  c := a div b;
  CheckEquals($2222222222 div $11, UInt64(c));

  a.Init($2222222222222222, $2222222222);
  b := $11;
  c := a div b;
  CheckEquals($0202020202020202, c.Lo);
  CheckEquals($202020202, c.Hi);
end;

procedure TUInt128Tests.LongDiv;
var a, b, c: TUInt128;
begin
  a.Init($2222222222222222, $22222222);
  b.Init($1111111111111111, $1111);
  c := a div b;
  CheckEquals($20000, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, $7000000000000000);
  b.Init(0, 2);
  c := a div b;
  CheckEquals($3800000000000000, c.Lo);
  CheckEquals(0, c.Hi);

  a.Init(0, 1);
  b.Init(0, 2);
  c := a div b;
  CheckEquals(0, c.Lo);
  CheckEquals(0, c.Hi);
end;

procedure TUInt128Tests.Less;
var a, b: TUInt128;
begin
  a := 2;
  b := 3;
  CheckTrue(a < b);
  CheckFalse(b < a);

  a.Init(0, 2);
  b.Init(0, 3);
  CheckTrue(a < b);
  CheckFalse(b < a);

  a.Init(2, 0);
  b.Init(0, 3);
  CheckTrue(a < b);
  CheckFalse(b < a);
end;

procedure TUInt128Tests.Incr;
var a: TUInt128;
begin
  a := 4;
  Inc(a);
  CheckEquals(5, a.Lo);
  CheckEquals(0, a.Hi);

  a.Init(0, 4);
  Inc(a);
  CheckEquals(1, a.Lo);
  CheckEquals(4, a.Hi);

  a.Init(High(UInt64), 4);
  Inc(a);
  CheckEquals(0, a.Lo);
  CheckEquals(5, a.Hi);
end;

{$ifopt Q+}

procedure TUInt128Tests.AddOverflow;
var a: TUInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(UInt64));
  a := a + a;
end;

procedure TUInt128Tests.SubOverflow;
var a, b: TUInt128;
begin
  ExpectedException := EIntOverflow;
  a := 2;
  b := 10;
  a := a - b;
end;

procedure TUInt128Tests.MulOverflow;
var a: TUInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(Int64));
  a := a * a;
end;

procedure TUInt128Tests.IncOverflow;
var a: TUInt128;
begin
  ExpectedException := EIntOverflow;
  a.Init(High(UInt64), High(UInt64));
  Inc(a);
end;

{$endif}

{$endregion}

{$region 'TReal128Tests'}

procedure TReal128Tests.FromDouble;
var r: TReal128;
    d: Double;
begin
  d := 1/2;
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($3FFE000000000000, r.Hi);

  d := 1/4;
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($3FFD000000000000, r.Hi);

  d := -1/2;
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($BFFE000000000000, r.Hi);

  d := 2;
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($4000000000000000, r.Hi);
end;

procedure TReal128Tests.FromDoubleSubnormal;
var r: TReal128;
    d: Double;
    i: UInt64 absolute d;
begin
  i := $0008000000000000; // 2^-1023
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($3C00000000000000, r.Hi);

  i := $0008000000000001; // 2^-1023 + 2^-1074
  r.Init(d);
  CheckEquals($2000000000000000, r.Lo);
  CheckEquals($3C00000000000000, r.Hi);

  // the smallest subnormal number
  i := 1; // 2^-1074
  r.Init(d);
  CheckEquals(0, r.Lo);
  CheckEquals($3BCD000000000000, r.Hi);
end;

procedure TReal128Tests.RealToDbl_DblPrec;
var r: TReal128;
    d: Double;
begin
  d := 1;
  r.Init(d);
  d := r.AsDouble;
  CheckEquals(1, d);

  d := 1/2;
  r.Init(d);
  d := r.AsDouble;
  CheckEquals(1/2, d);

  d := -1/2;
  r.Init(d);
  d := r.AsDouble;
  CheckEquals(-1/2, d);

  d := 3/4;
  r.Init(d);
  d := r.AsDouble;
  CheckEquals(3/4, d);
end;

procedure TReal128Tests.RealToDbl_SubDblPrec;
var r: TReal128;
    d: Double;
    i: UInt64 absolute d;
begin
  r.Init($0800000000000000, $3FFF000000000000); // 1 + 2^-54
  d := r.AsDouble;
  CheckEquals($3FF0000000000000, i); // rounded down

  r.Init($0C00000000000000, $3FFF000000000000); // 1 + 2^-53 + 2^-54
  d := r.AsDouble;
  CheckEquals($3FF0000000000001, i); // rounded up

  r.Init($0800000000000000, $3FFF000000000000); // 1 + 2^-53
  d := r.AsDouble;
  CheckEquals($3FF0000000000000, i); // round down due to tie to even

  r.Init($1800000000000000, $3FFF000000000000); // 1 + 2^-52 + 2^-53
  d := r.AsDouble;
  CheckEquals($3FF0000000000002, i); // rounded up due to tie to even

  // rounding up overflow
  r.Init($F800000000000000, $3FFFFFFFFFFFFFFF); // Sum(2^-i, {i, 0, 53})
  d := r.AsDouble;
  CheckEquals($4000000000000000, i);
end;

procedure TReal128Tests.RealToDbl_DblSubnormal;
var r: TReal128;
    d: Double;
var i: UInt64 absolute d;
begin
  r.Init(0, $3C00000000000000); // 2^-1023
  d := r.AsDouble;
  CheckEquals($0008000000000000, i);

  // double LSB
  r.Init($2000000000000000, $3C00000000000000); // 2^-1023 + 2^-1074
  d := r.AsDouble;
  CheckEquals($0008000000000001, i);

  // rounding up
  r.Init($1800000000000000, $3C00000000000000); // 2^-1023 + 2^-1075 + 2^-1076
  d := r.AsDouble;
  CheckEquals($0008000000000001, i);

  r.Init($1080000000000000, $3C00000000000000); // 2^-1023 + 2^-1075 + 2^-1080
  d := r.AsDouble;
  CheckEquals($0008000000000001, i);

  // rounding down
  r.Init($0800000000000000, $3C00000000000000); // 2^-1023 + 2^-1076
  d := r.AsDouble;
  CheckEquals($0008000000000000, i);

  // the smallest subnormal number
  r.Init(0, $3BCD000000000000); // 2^-1074
  d := r.AsDouble;
  CheckEquals(1, i);

  // less than double zero
  r.Init(0, $3BCC000000000000); // 2^-1075
  d := r.AsDouble;
  CheckEquals(0, i);

  // rounding up overflow
  r.Init($F000000000000000, $3C00FFFFFFFFFFFF); // Sum(2^-i, {i, 1023, 1075})
  d := r.AsDouble;
  CheckEquals($0010000000000000, i);

  // negative number
  r.Init($F000000000000000, $BC00FFFFFFFFFFFF); // -Sum(2^-i, {i, 1023, 1075})
  d := r.AsDouble;
  CheckEquals($8010000000000000, i);
end;

procedure TReal128Tests.RealToDbl_ExpOF;
var r: TReal128;
begin
  r.Init(0, $43FF000000000000); // 2^1024
  ExpectedException := ERangeError;
  r.AsDouble;
end;

procedure TReal128Tests.RealToDbl_RoundUpWithExpOF;
var r: TReal128;
begin
  r.Init($F800000000000000, $43FEFFFFFFFFFFFF); // Sum(2^-(1023+i), {i, 0, 53})
  ExpectedException := ERangeError;
  r.AsDouble;
end;

procedure TReal128Tests.AddDblPP;
var a, b, c: TReal128;
    d: Double;
begin
  a := 1 + 1/2;
  b := 1/4;
  c := a + b; // addition without overflow
  d := c.AsDouble;
  CheckEquals(1.75, d);

  b := 1 + 1/4;
  c := a + b;  // addition with overflow
  d := c.AsDouble;
  CheckEquals(2.75, d);
end;

procedure TReal128Tests.Add_SubDblPrec;
var a, b, c: TReal128;
begin
  a.Init($0800000000000000 , $3FFF000000000000); // 1 + 2^-53
  c := a + a;
  CheckEquals($0800000000000000, c.Lo);
  CheckEquals($4000000000000000, c.Hi);

  a.Init(1, $3FFF000000000000); // 1 + 2^-112
  c := a + a;
  CheckEquals(1, c.Lo);
  CheckEquals($4000000000000000, c.Hi);

  b.Init(0, $3F8F000000000000); // 2^-122
  c := a + b;
  CheckEquals(2, c.Lo);
  CheckEquals($3FFF000000000000, c.Hi);
end;

procedure TReal128Tests.Add_RoundUpWthoutOF;
var a, b, c: TReal128;
begin
  a.Init(High(UInt64) - 1, $3FFFFFFFFFFFFFFF); // Sum(2^-i, {i, 0, 111})
  b.Init(0, $3F8E800000000000); // 2^-113 + 2^-114 -> Sticky > 1/2
  c := a + b;
  CheckEquals(High(UInt64), c.Lo);
  CheckEquals($3FFFFFFFFFFFFFFF, c.Hi);
end;

procedure TReal128Tests.Add_TieToEvenWithoutOF;
var a, b, c: TReal128;
begin
  a.Init(High(UInt64) - 2, $3FFFFFFFFFFFFFFF); // Sum(2^-i, {i, 0, 110}) + 2^-111
  b.Init(0, $3F8E000000000000); // 2^-113 -> Sticky = 1/2
  c := a + b;
  CheckEquals(High(UInt64) - 1, c.Lo);
  CheckEquals($3FFFFFFFFFFFFFFF, c.Hi);
end;

procedure TReal128Tests.Add_RoundUpWithOF;
var a, b, c: TReal128;
begin
  a.Init(High(UInt64), $3FFFFFFFFFFFFFFF); // Sum(2^-i, {i, 0, 112})
  b.Init(0, $3F8E800000000000); // 2^-113 + 2^-114 -> Sticky > 1/2
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals($4000000000000000, c.Hi);
end;

procedure TReal128Tests.Add_TieToEvenWithOF;
var a, b, c: TReal128;
begin
  a.Init(High(UInt64), $3FFFFFFFFFFFFFFF); // Sum(2^-i, {i, 0, 112})
  b.Init(0, $3F8E000000000000); // 2^-113  (Sticky = 1/2)
  c := a + b;
  CheckEquals(0, c.Lo);
  CheckEquals($4000000000000000, c.Hi);
end;

procedure TReal128Tests.Sub_SameExp;
var a, b, c: TReal128;
begin
  a := 1 + 1/8;
  b := 1 + 1/16;
  c := a - b;
  CheckEquals(0, c.Lo);
  CheckEquals($3FFB000000000000, c.Hi);
end;

procedure TReal128Tests.MulDbl;
var a, b, c: TReal128;
    d: Double;
begin
  a := 1/2;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(1/8, d);

  a := 1/8;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(1/32, d);

  a := 1 + 1/2;
  b := 1/4;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(3/8, d);

  a := 1 + 1/2;
  b := 1 + 1/2;
  c := a * b;
  d := c.AsDouble;
  CheckEquals(9/4, d);
end;

procedure TReal128Tests.Reciprocal;
var a: TReal128;
    d: Double;
begin
  a := 2;
  a := a.Reciprocal;
  d := a.AsDouble;
  CheckEquals(0.5, d);

  a := 6;
  a := a.Reciprocal;
  d := a.AsDouble;
  CheckEquals(0.166666666666667, d, 1e-15);
  CheckEquals($5555555555555555, a.Lo);
  CheckEquals($3FFC555555555555, a.Hi);

  a := 7;
  a := a.Reciprocal;
  CheckEquals($2492492492492492, a.Lo);
  CheckEquals($3ffc249249249249, a.Hi);
end;

procedure TReal128Tests.DivideDbl;
var a, b, c: TReal128;
    d: Double;
begin
  a := 1/4;
  b := 1/2;
  c := a / b;
  d := b.AsDouble;
  CheckEquals(1/2, d);
end;

procedure TReal128Tests.Sqrt;
var a: TReal128;
begin
  a := 2;
  a := a.Sqrt;
  CheckEquals($C908B2FB1366EA95, a.Lo);
  CheckEquals($3FFF6A09E667F3BC, a.Hi);
end;

{$endregion}

{$region 'TCmplx256Tests'}

procedure TCmplx256Tests.AddDbl;
var a, b, c: TCmplx256;
begin

  c := a + b;

end;

procedure TCmplx256Tests.SubDbl;
var a, b, c: TCmplx256;
begin

  c := a - b;

end;

{$endregion}

initialization

  RegisterTest(TLowLvlTests.Suite);
  RegisterTest(TInt128Tests.Suite);
  RegisterTest(TUInt128Tests.Suite);
  RegisterTest(TReal128Tests.Suite);
  RegisterTest(TCmplx256Tests.Suite);

end.
