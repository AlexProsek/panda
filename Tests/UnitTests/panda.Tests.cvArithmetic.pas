unit panda.Tests.cvArithmetic;

interface

uses
    TestFramework
  , panda.Nums
  , panda.Tests.NDATestCase
  , panda.cvArithmetic
  , System.SysUtils
  ;

type
  TTestVectorMath = class(TNDATestCase)
  protected const
    stol = 1e-6;
    dtol = 1e-12;
  published
    procedure MovDupI8;
    procedure AddVec1_Double;
    procedure AddVec3_Single;
    procedure AddVec3_Double;
    procedure AddVec4_Single;
    procedure AddVec4_Double;
    procedure AddVecInPlace_Single;
    procedure AddVecInPlace_bs2_Single;
    procedure SubVec3_Single;
    procedure SubVec8_Single;
    procedure AddScalarToVec1_Double;
    procedure AddScalarToVec2_Double;
    procedure AddScalarToVec3_Double;
    procedure AddScalarToVec3_Single;
    procedure AddScalarToVec4_Single;
    procedure AddScalarToVec5_Single;
    procedure AddScalarToVec2_Cmplx128;
    procedure SubScalarFromVec3_Single;
    procedure SubScalarFromVec5_Single;
    procedure SubFromScalarVec3_Single;
    procedure SubFromScalarVec6_Single;
    procedure SubFromScalarVec5_Single;
    procedure AddVecWithSat3_UInt8;
    procedure AddVecWithSat16_UInt8;
    procedure AddVecWithSat3_UInt16;
    procedure AddVecWithSat8_UInt16;
    procedure AddScalarWithSat3_UInt8;
    procedure AddScalarWithSat16_UInt8;
    procedure AddScalarWithSat3_UInt16;
    procedure AddScalarWithSat8_UInt16;
    procedure SubVecWithSat3_UInt8;
    procedure SubVecWithSat16_UInt8;
    procedure SubVecWithSat3_UInt16;
    procedure SubVecWithSat8_UInt16;
    procedure SubScalarWithSat3_UInt8;
    procedure SubScalarWithSat16_UInt8;
    procedure SubScalarWithSat3_UInt16;
    procedure SubScalarWithSat8_UInt16;
    procedure MulVec1_Cmplx128;
    procedure MulVec2_Cmplx128;
    procedure MulByScalar3_Single;
    procedure MulByScalar5_Single;
    procedure MulbyScalar1_Cmplx128;
    procedure MulbyScalar2_Cmplx128;
    procedure DivVec1_Cmplx128;
    procedure DivVec2_Cmplx128;
    procedure TestSatAdd_U8;
    procedure TestSatSub_U8;
    procedure TestMin_2U8;
    procedure TestMin_3U8;
    procedure TestMin_4U8;
    procedure TestMin_NU8;
    procedure TestMax_2U8;
    procedure TestScal_3_Single;
    procedure TestScal_8_Single;
    procedure TestScal_10_Single;
    procedure TestScal_3_Double;
    procedure TestScal_8_Double;
    procedure TestAXPY_1;
    procedure TestAXPY_3;
    procedure TestAXPY_4;
    procedure TestAXPY_5;
    procedure TestAXPY_6;
    procedure TestAXPY_3_Single;
    procedure TestAXPY_4_Single;
    procedure TestAXPY_6_Single;
  {$ifdef AVX}
    procedure TestAXPY_8_Single;
    procedure TestAXPY_9_Single;
  {$endif}
    procedure TestAXPY_A1;
    procedure TestAXPY_1_DblCmplx;
    procedure TestAXPY_2_DblCmplx;
    procedure TestDot4;
    procedure TestDot5;
    procedure TestDDot5_YStep2;
    procedure TestDot4_Single;
    procedure TestDot6_Single;
    procedure TestDot8_Single;
    procedure TestDot2_Cmplx;
    procedure TestSCopy_4;
    procedure TestSCopy_5;
    procedure TestSCopyConst;
    procedure TestSCopy_3_Double;
    procedure TestSCopy_4_Double;
    procedure TestSCopy_5_Double;
    procedure TestDiff_Double;
    procedure TEstDiff3_Single;
    procedure TestDiff4_Single;
    procedure TestDiffWithStep_Single_2;
    procedure TestDiffWithStep_Single_4;
    procedure TestDiffWithStep_Single_6;
    procedure TestDiffWithStep_Single_8;
    procedure TestDiffWithStep_Double;
    procedure TestVecNeg_Single_3;
    procedure TestVecNeg_Single_8;
    procedure TestVecNeg_Single_9;
    procedure TestVecAnd_3;
    procedure TestVecAnd_4;
    procedure TestVecAnd_5;
    procedure TestVecAnd_16;
    procedure TestVecAnd_17;
    procedure TestVecOr_4;
  end;

implementation

type
  PSingle = System.PSingle;

{$region 'TTestVectorMath'}

procedure _dup_I8_xmm0(value: Byte);
asm
  mov dl, al
  shl edx, 8
  mov dl, al
  mov ax, dx
  shl eax, 16
  mov ax, dx
  mov [esp - 4], eax
  mov [esp - 8], eax
  movddup xmm0, [esp - 8]
end;

procedure test(value: Byte);
asm
  call _dup_I8_xmm0
end;

procedure TTestVectorMath.MovDupI8;
begin
  test(27);
end;

procedure TTestVectorMath.AddVec1_Double;
var A, B, res: Double;
begin
  A := 1; B := 2; res := 0;
  VecAdd(PDouble(@A), @B, @res, 1);
  CheckEquals(3, res);
end;

procedure TTestVectorMath.AddVec3_Single;
const
  A: array [0..2] of Single = (1, 2, 3);
  B: array [0..2] of Single = (2, 4, 6);
var res: array [0..2] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PSingle(@A[0]), @B[0], @res[0], 3);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
end;

procedure TTestVectorMath.AddVec3_Double;
const
  A: array [0..2] of Double = (1, 2, 3);
  B: array [0..2] of Double = (2, 4, 6);
var res: array [0..2] of Double;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PDouble(@A[0]), @B[0], @res[0], 3);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
end;

procedure TTestVectorMath.AddVec4_Single;
const
  A: array [0..3] of Single = (1, 2, 3, 4);
  B: array [0..3] of Single = (2, 4, 6, 8);
var res: array [0..3] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PSingle(@A[0]), @B[0], @res[0], 4);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
  CheckEquals(12, res[3]);
end;

procedure TTestVectorMath.AddVec4_Double;
const
  A: array [0..3] of Double = (1, 2, 3, 4);
  B: array [0..3] of Double = (2, 4, 6, 8);
var res: array [0..3] of Double;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PDouble(@A[0]), @B[0], @res[0], 4);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
  CheckEquals(12, res[3]);
end;

procedure TTestVectorMath.AddVecInPlace_Single;
const
  A: array [0..3] of Single = (1, 2, 3, 4);
  B: array [0..3] of Single = (2, 4, 6, 8);
var res: array [0..3] of Single;
begin
  Move(A, res, SizeOf(A));
  VecAddInPlace(PSingle(@res[0]), PSingle(@B[0]), 1, 1, 4);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
  CheckEquals(12, res[3]);
end;

procedure TTestVectorMath.AddVecInPlace_bs2_Single;
const
  A: array [0..3] of Single = (1, 2, 3, 4);
  B: array [0..7] of Single = (2, 0, 4, 0, 6, 0, 8, 0);
var res: array [0..3] of Single;
begin
  Move(A, res, SizeOf(A));
  VecAddInPlace(PSingle(@res[0]), PSingle(@B[0]), 1, 2, 4);
  CheckEquals(3, res[0]);
  CheckEquals(6, res[1]);
  CheckEquals(9, res[2]);
  CheckEquals(12, res[3]);
end;

procedure TTestVectorMath.SubVec3_Single;
const
  A: array [0..2] of Single = (2, 4, 6);
  B: array [0..2] of Single = (1, 2, 3);
var res: array [0..2] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(PSingle(@A[0]), @B[0], @res[0], 3);
  CheckEquals(1, res[0]);
  CheckEquals(2, res[1]);
  CheckEquals(3, res[2]);
end;

procedure TTestVectorMath.SubVec8_Single;
const
  A: array [0..7] of Single = (2, 4, 6, 8, 10, 12, 14, 16);
  B: array [0..7] of Single = (1, 2, 3, 4,  5,  6,  7,  8);
var res: array [0..7] of Single;
    I: Integer;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(PSingle(@A[0]), @B[0], @res[0], 8);
  for I := 0 to High(res) do
    CheckEquals(I + 1, res[I], stol);
end;

procedure TTestVectorMath.AddScalarToVec1_Double;
var A, B, res: Double;
begin
  A := 1; B := 2; res := 0;
  VecAdd(PDouble(@A), B, @res, 1);
  CheckEquals(3, res);
end;

procedure TTestVectorMath.AddScalarToVec2_Double;
const
  A: array [0..1] of Double = (1, 2);
var res: array [0..1] of Double;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PDouble(@A[0]), 2, @res[0], 2);
  CheckEquals(3, res[0]);
  CheckEquals(4, res[1]);
end;

procedure TTestVectorMath.AddScalarToVec3_Double;
const
  A: array [0..2] of Double = (1, 2, 3);
var res: array [0..2] of Double;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PDouble(@A[0]), 2, @res[0], 3);
  CheckEquals(3, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(5, res[2]);
end;

procedure TTestVectorMath.AddScalarToVec3_Single;
const
  A: array [0..2] of Single = (1, 2, 3);
var res: array [0..2] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PSingle(@A[0]), 2, @res[0], 3);
  CheckEquals(3, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(5, res[2]);
end;

procedure TTestVectorMath.AddScalarToVec4_Single;
const
  A: array [0..3] of Single = (1, 2, 3, 4);
var res: array [0..3] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PSingle(@A[0]), 2, @res[0], 4);
  CheckEquals(3, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(5, res[2]);
  CheckEquals(6, res[3]);
end;

procedure TTestVectorMath.AddScalarToVec5_Single;
const
  A: array [0..4] of Single = (1, 2, 3, 4, 5);
var res: array [0..4] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecAdd(PSingle(@A[0]), 2, @res[0], 5);
  CheckEquals(3, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(5, res[2]);
  CheckEquals(6, res[3]);
  CheckEquals(7, res[4]);
end;

procedure TTestVectorMath.AddScalarToVec2_Cmplx128;
const
  A: array [0..1] of TCmplx128 = ((Re: 1; Im: 2), (Re: 2; Im: 1));
var res: array [0..1] of TCmplx128;
    B: TCmplx128;
begin
  FillChar(res,  SizeOf(res), 0);
  B.Init(3, 4);
  VecAdd(PCmplx128(@A[0]), B, @res[0], 2);
  CheckEquals(4, res[0].Re);
  CheckEquals(6, res[0].Im);
  CheckEquals(5, res[1].Re);
  CheckEquals(5, res[1].Im);
end;

procedure TTestVectorMath.SubScalarFromVec3_Single;
const
  A: array [0..2] of Single = (1, 2, 3);
var res: array [0..2] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(PSingle(@A[0]), 2, @res[0], Length(A));
  CheckEquals(-1, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
end;

procedure TTestVectorMath.SubScalarFromVec5_Single;
const
  A: array [0..4] of Single = (1, 2, 3, 4, 5);
var res: array [0..4] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(PSingle(@A[0]), 2, @res[0], Length(A));
  CheckEquals(-1, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
  CheckEquals(2, res[3]);
  CheckEquals(3, res[4]);
end;

procedure TTestVectorMath.SubFromScalarVec3_Single;
const
  A: array [0..2] of Single = (1, 2, 3);
var res: array [0..2] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(2, PSingle(@A[0]), @res[0], Length(A));
  CheckEquals(1, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(-1, res[2]);
end;

procedure TTestVectorMath.SubFromScalarVec6_Single;
const
  A: array [0..3] of Single = (1, 2, 3, 4);
var res: array [0..3] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(2, PSingle(@A[0]), @res[0], Length(A));
  CheckEquals(1, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(-1, res[2]);
  CheckEquals(-2, res[3]);
end;

procedure TTestVectorMath.SubFromScalarVec5_Single;
const
  A: array [0..4] of Single = (1, 2, 3, 4, 5);
var res: array [0..4] of Single;
begin
  FillChar(res, SizeOf(res), 0);
  VecSub(2, PSingle(@A[0]), @res[0], Length(A));
  CheckEquals(1, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(-1, res[2]);
  CheckEquals(-2, res[3]);
  CheckEquals(-3, res[4]);
end;

procedure TTestVectorMath.AddVecWithSat3_UInt8;
const A: array [0..2] of UInt8 = (1, 2, 3);
      B: array [0..2] of UInt8 = (253, 253, 253);
var res: array [0..2] of UInt8;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt8(@A[0]), @B[0], @res[0], 3);
  CheckEquals(254, res[0]);
  CheckEquals(255, res[1]);
  CheckEquals(255, res[2]);
end;

procedure TTestVectorMath.AddVecWithSat16_UInt8;
const
  A: array [0..15] of UInt8 = (1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0);
  B: array [0..15] of UInt8 = (250, 250, 250, 250, 250, 250, 250, 250, 1, 2, 3, 4, 5, 6, 7, 8);
var res: array [0..15] of UInt8;
    I: Integer;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt8(@A[0]), @B[0], @res[0], 16);
  CheckEquals(251, res[0]);
  CheckEquals(252, res[1]);
  CheckEquals(253, res[2]);
  CheckEquals(254, res[3]);
  CheckEquals(255, res[4]);
  CheckEquals(255, res[5]);
  CheckEquals(255, res[6]);
  CheckEquals(255, res[7]);
  for I := 1 to 8 do
    CheckEquals(I, res[7 + I]);
end;

procedure TTestVectorMath.AddVecWithSat3_UInt16;
const A: array [0..2] of UInt16 = (1, 2, 3);
      B: array [0..2] of UInt16 = ($FFFD, $FFFD, $FFFD);
var res: array [0..2] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt16(@A[0]), @B[0], @res[0], 3);
  CheckEquals($FFFE, res[0]);
  CheckEquals($FFFF, res[1]);
  CheckEquals($FFFF, res[2]);
end;

procedure TTestVectorMath.AddVecWithSat8_UInt16;
const
  A: array [0..7] of UInt16 = (1, 2, 3, 4, 5, 6, 7, 8);
  B: array [0..7] of UInt16 = ($FFFB, $FFFB, $FFFB, $FFFB, $FFFB, $FFFB, $FFFB, $FFFB);
var res: array [0..7] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt16(@A[0]), @B[0], @res[0], 8);
  CheckEquals($FFFC, res[0]);
  CheckEquals($FFFD, res[1]);
  CheckEquals($FFFE, res[2]);
  CheckEquals($FFFF, res[3]);
  CheckEquals($FFFF, res[4]);
  CheckEquals($FFFF, res[5]);
  CheckEquals($FFFF, res[6]);
  CheckEquals($FFFF, res[7]);
end;

procedure TTestVectorMath.AddScalarWithSat3_UInt8;
const A: array [0..2] of UInt8 = (1, 2, 3);
var res: array [0..2] of UInt8;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt8(@A[0]), 253, @res[0], 3);
  CheckEquals(254, res[0]);
  CheckEquals(255, res[1]);
  CheckEquals(255, res[2]);
end;

procedure TTestVectorMath.AddScalarWithSat16_UInt8;
const
  A: array [0..15] of UInt8 = (1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0);
var res: array [0..15] of UInt8;
    I: Integer;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt8(@A[0]), 250, @res[0], 16);
  CheckEquals(251, res[0]);
  CheckEquals(252, res[1]);
  CheckEquals(253, res[2]);
  CheckEquals(254, res[3]);
  CheckEquals(255, res[4]);
  CheckEquals(255, res[5]);
  CheckEquals(255, res[6]);
  CheckEquals(255, res[7]);
  for I := 1 to 8 do
    CheckEquals(250, res[7 + I]);
end;

procedure TTestVectorMath.AddScalarWithSat3_UInt16;
const A: array [0..2] of UInt16 = (1, 2, 3);
var res: array [0..2] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt16(@A[0]), $FFFD, @res[0], 3);
  CheckEquals($FFFE, res[0]);
  CheckEquals($FFFF, res[1]);
  CheckEquals($FFFF, res[2]);
end;

procedure TTestVectorMath.AddScalarWithSat8_UInt16;
const
  A: array [0..7] of UInt16 = (1, 2, 3, 4, 5, 6, 7, 8);
var res: array [0..7] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecAddWithSat(PUInt16(@A[0]), $FFFB, @res[0], 8);
  CheckEquals($FFFC, res[0]);
  CheckEquals($FFFD, res[1]);
  CheckEquals($FFFE, res[2]);
  CheckEquals($FFFF, res[3]);
  CheckEquals($FFFF, res[4]);
  CheckEquals($FFFF, res[5]);
  CheckEquals($FFFF, res[6]);
  CheckEquals($FFFF, res[7]);
end;

procedure TTestVectorMath.SubVecWithSat3_UInt8;
const A: array [0..2] of UInt8 = (1, 2, 3);
      B: array [0..2] of UInt8 = (2, 2, 2);
var res: array [0..2] of UInt8;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt8(@A[0]), @B[0], @res[0], 3);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
end;

procedure TTestVectorMath.SubVecWithSat16_UInt8;
const
  A: array [0..15] of UInt8 = (1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0);
  B: array [0..15] of UInt8 = (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2);
var res: array [0..15] of UInt8;
    I: Integer;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt8(@A[0]), @B[0], @res[0], 16);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
  CheckEquals(2, res[3]);
  CheckEquals(3, res[4]);
  CheckEquals(4, res[5]);
  CheckEquals(5, res[6]);
  CheckEquals(6, res[7]);
  for I := 1 to 8 do
    CheckEquals(0, res[7 + I]);
end;

procedure TTestVectorMath.SubVecWithSat3_UInt16;
const A: array [0..2] of UInt16 = (1, 2, 3);
      B: array [0..2] of UInt16 = (2, 2, 2);
var res: array [0..2] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt16(@A[0]), @B[0], @res[0], 3);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
end;

procedure TTestVectorMath.SubVecWithSat8_UInt16;
const
  A: array [0..7] of UInt16 = (1, 2, 3, 4, 5, 6, 7, 8);
  B: array [0..7] of UInt16 = (3, 3, 3, 3, 3, 3, 3, 3);
var res: array [0..7] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt16(@A[0]), @B[0], @res[0], 8);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(0, res[2]);
  CheckEquals(1, res[3]);
  CheckEquals(2, res[4]);
  CheckEquals(3, res[5]);
  CheckEquals(4, res[6]);
  CheckEquals(5, res[7]);
end;

procedure TTestVectorMath.SubScalarWithSat3_UInt8;
const A: array [0..2] of UInt8 = (1, 2, 3);
var res: array [0..2] of UInt8;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt8(@A[0]), 2, @res[0], 3);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
end;

procedure TTestVectorMath.SubScalarWithSat16_UInt8;
const
  A: array [0..15] of UInt8 = (1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0);
var res: array [0..15] of UInt8;
    I: Integer;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt8(@A[0]), 3, @res[0], 16);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(0, res[2]);
  CheckEquals(1, res[3]);
  CheckEquals(2, res[4]);
  CheckEquals(3, res[5]);
  CheckEquals(4, res[6]);
  CheckEquals(5, res[7]);
  for I := 1 to 8 do
    CheckEquals(0, res[7 + I]);
end;

procedure TTestVectorMath.SubScalarWithSat3_UInt16;
const A: array [0..2] of UInt16 = (1, 2, 3);
var res: array [0..2] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt16(@A[0]), 2, @res[0], 3);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(1, res[2]);
end;

procedure TTestVectorMath.SubScalarWithSat8_UInt16;
const
  A: array [0..7] of UInt16 = (1, 2, 3, 4, 5, 6, 7, 8);
var res: array [0..7] of UInt16;
begin
  FillChar(res, SizeOf(res), 0);
  VecSubWithSat(PUInt16(@A[0]), 3, @res[0], 8);
  CheckEquals(0, res[0]);
  CheckEquals(0, res[1]);
  CheckEquals(0, res[2]);
  CheckEquals(1, res[3]);
  CheckEquals(2, res[4]);
  CheckEquals(3, res[5]);
  CheckEquals(4, res[6]);
  CheckEquals(5, res[7]);
end;

procedure TTestVectorMath.MulVec1_Cmplx128;
var a, b, c: TCmplx128;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  VecMul(PCmplx128(@a), @b, @c, 1);
  CheckEquals(-5, c.Re, dtol);
  CheckEquals(10, c.Im, dtol);
end;

procedure TTestVectorMath.MulVec2_Cmplx128;
var a, b, c: array [0..1] of TCmplx128;
begin
  a[0].Init(1, 2);
  a[1].Init(3, 4);
  b[0].Init(3, 4);
  b[1].Init(5, 6);
  VecMul(PCmplx128(@a), @b, @c, 2);
  CheckEquals(-5, c[0].Re, dtol);
  CheckEquals(10, c[0].Im, dtol);
  CheckEquals(-9, c[1].Re, dtol);
  CheckEquals(38, c[1].Im, dtol);
end;

procedure TTestVectorMath.MulByScalar3_Single;
const
  A: array [0..2] of Single = (1, 2, 3);
var res: array [0..2] of Single;
begin
  VecMul(PSingle(@A[0]), 2, PSingle(@res[0]), Length(A));
  CheckEquals(2, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(6, res[2]);
end;

procedure TTestVectorMath.MulByScalar5_Single;
const
  A: array [0..4] of Single = (1, 2, 3, 4, 5);
var res: array [0..4] of Single;
begin
  VecMul(PSingle(@A[0]), 2, PSingle(@res[0]), Length(A));
  CheckEquals(2, res[0]);
  CheckEquals(4, res[1]);
  CheckEquals(6, res[2]);
  CheckEquals(8, res[3]);
  CheckEquals(10, res[4]);
end;

procedure TTestVectorMath.MulbyScalar1_Cmplx128;
var a, b, c: TCmplx128;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  VecMul(PCmplx128(@a), b, @c, 1);
  CheckEquals(-5, c.Re, dTol);
  CheckEquals(10, c.Im, dTol);
end;

procedure TTestVectorMath.MulbyScalar2_Cmplx128;
var a, c: array [0..1] of TCmplx128;
    b: TCmplx128;
begin
  a[0].Init(1, 2);
  a[1].Init(2, 3);
  b.Init(4, 5);
  VecMul(PCmplx128(@a), b, @c, 2);
  CheckEquals(-6, c[0].Re, dTol);
  CheckEquals(13, c[0].Im, dTol);
  CheckEquals(-7, c[1].Re, dTol);
  CheckEquals(22, c[1].Im, dTol);
end;

procedure TTestVectorMath.DivVec1_Cmplx128;
var a, b, c: TCmplx128;
begin
  a.Init(1, 2);
  b.Init(3, 4);
  VecDiv(PCmplx128(@a), @b, @c, 1);
  CheckEquals(0.44, c.Re, dTol);
  CheckEquals(0.08, c.Im, dTol);
end;

procedure TTestVectorMath.DivVec2_Cmplx128;
var a, b, c: array [0..1] of TCmplx128;
begin
  a[0].Init(1, 2);
  a[1].Init(3, 4);
  b[0].Init(3, 4);
  b[1].Init(5, 6);
  VecDiv(PCmplx128(@a), @b, @c, 2);
  CheckEquals(0.44, c[0].Re, dTol);
  CheckEquals(0.08, c[0].Im, dTol);
  CheckEquals(0.639344262295082,  c[1].Re, dTol);
  CheckEquals(0.0327868852459016, c[1].Im, dTol);
end;

procedure TTestVectorMath.TestSatAdd_U8;
var a, b, res: UInt8;
begin
  a := 250; b := 100;
  res := SatAdd_U8(a, b);
  CheckEquals(255, res);

  a:= 2; b := 3;
  res := SatAdd_U8(a, b);
  CheckEquals(5, res);

  a := 0; b := 255;
  res := SatAdd_U8(a, b);
  CheckEquals(255, res);
end;

procedure TTestVectorMath.TestSatSub_U8;
var a, b, res: UInt8;
begin
  a := 5; b := 2;
  res := SatSub_U8(a, b);
  CheckEquals(3, res);
  res := SatSub_U8(b, a);
  CheckEquals(0, res);

  a := 0; b := 255;
  res := SatSub_U8(a, b);
  CheckEquals(0, res);
  res := SatSub_U8(b, a);
  CheckEquals(255, res);
end;

procedure TTestVectorMath.TestMin_2U8;
var a, b, res: UInt8;
begin
  a := 5; b := 2;
  res := Min_2U8(a, b);
  CheckEquals(b, res);
  res := Min_2U8(b, a);
  CheckEquals(b, res);

  res := Min_2U8(a, a);
  CheckEquals(a, res);

  a := 0; b := 255;
  res := Min_2U8(a, b);
  CheckEquals(0, res);
  res := Min_2U8(b, a);
  CheckEquals(0, res);
end;

procedure TTestVectorMath.TestMin_3U8;
var a, b, c, res: UInt8;
begin
  a := 5; b := 2; c := 8;
  res := Min_3U8(a, b, c);
  CheckEquals(b, res);
  res := Min_3U8(a, c, b);
  CheckEquals(b, res);
  res := Min_3U8(b, a, c);
  CheckEquals(b, res);
  res := Min_3U8(b, c, a);
  CheckEquals(b, res);
  res := Min_3U8(c, a, b);
  CheckEquals(b, res);
  res := Min_3U8(c, b, a);
  CheckEquals(b, res);
end;

procedure TTestVectorMath.TestMin_4U8;
var a, b, c, d, res: UInt8;
begin
  a := 5; b := 2; c := 8; d:= 3;
  res := Min_4U8(a, b, c, d);
  CheckEquals(b, res);
  res := Min_4U8(a, c, b, d);
  CheckEquals(b, res);
  res := Min_4U8(b, a, c, d);
  CheckEquals(b, res);
  res := Min_4U8(b, c, a, d);
  CheckEquals(b, res);
  res := Min_4U8(c, a, b, d);
  CheckEquals(b, res);
  res := Min_4U8(c, b, a, d);
  CheckEquals(b, res);
  res := Min_4U8(d, a, b, c);
  CheckEquals(b, res);
  res := Min_4U8(a, d, c, b);
  CheckEquals(b, res);
  res := Min_4U8(a, b, d, c);
  CheckEquals(b, res);
  res := Min_4U8(b, c, d, a);
  CheckEquals(b, res);
end;

procedure TTestVectorMath.TestMin_NU8;
var x: TArray<UInt8>;
    res: UInt8;
begin
  x := TArray<UInt8>.Create(
    5, 4, 3, 2, 1, 2, 3, 4, 5, 6, 7, 8,
    7, 6, 5, 4, 3, 2
  );

  res := Min_NU8(PUInt8(x), 2);
  CheckEquals(4, res);

  res := Min_NU8(PUInt8(x), Length(x));
  CheckEquals(1, res);

  x[0] := 1; x[4] := 5;
  res := Min_NU8(PUInt8(x), Length(x));
  CheckEquals(1, res);

  x[0] := 5;
  res := Min_NU8(PUInt8(x), Length(x));
  CheckEquals(2, res);

  x[High(x)] := 1;
  res := Min_NU8(PUInt8(x), Length(x));
  CheckEquals(1, res);
end;

procedure TTestVectorMath.TestMax_2U8;
var a, b, res: UInt8;
begin
  a := 5; b := 2;
  res := Max_2U8(a, b);
  CheckEquals(a, res);
  res := Max_2U8(b, a);
  CheckEquals(a, res);
  res := Max_2U8(a, a);
  CheckEquals(a, res);
end;

procedure TTestVectorMath.TestScal_3_Single;
var src: TArray<Single>;
begin
  src := TArray<Single>.Create(1, 2, 3);
  sscal(PSingle(src), 3, 2);
  CheckEquals(2, src[0], stol);
  CheckEquals(4, src[1], stol);
  CheckEquals(6, src[2], stol);
end;

procedure TTestVectorMath.TestScal_8_Single;
const c: array [0..7] of Single = (1, 2, 3, 4, 5, 6, 7, 8);
var src: array [0..7] of Single;
    I, count: Integer;
begin
  count := Length(c);
  Move(c, src, count * SizeOf(Single));
  sscal(PSingle(@src[0]), count, 2);
  for I := 0 to count - 1 do
    CheckEquals(2 * c[I], src[I], stol);
end;

procedure TTestVectorMath.TestScal_10_Single;
const c: array [0..9] of Single = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
var src: array [0..9] of Single;
    I, count: Integer;
begin
  count := Length(c);
  Move(c, src, count * SizeOf(Single));
  sscal(PSingle(@src[0]), count, 2);
  for I := 0 to count - 1 do
    CheckEquals(2 * c[I], src[I], stol);
end;

procedure TTestVectorMath.TestScal_3_Double;
var src: TArray<Double>;
begin
  src := TArray<Double>.Create(1, 2, 3);
  dscal(PDouble(src), 3, 2);
  CheckEquals(2, src[0], dtol);
  CheckEquals(4, src[1], dtol);
  CheckEquals(6, src[2], dtol);
end;

procedure TTestVectorMath.TestScal_8_Double;
const c: array [0..7] of Double = (1, 2, 3, 4, 5, 6, 7, 8);
var src: array [0..7] of Double;
    I, count: Integer;
begin
  count := Length(c);
  Move(c, src, count * SizeOf(Double));
  dscal(PDouble(@src[0]), count, 2);
  for I := 0 to count - 1 do
    CheckEquals(2 * c[I], src[I], dtol);
end;

procedure TTestVectorMath.TestAXPY_1;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1);
  y := TArray<Double>.Create(3);
  axpy(2, PDouble(x), PDouble(y), 1);
  CheckEquals(5, y[0]);
end;

procedure TTestVectorMath.TestAXPY_3;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3);
  y := TArray<Double>.Create(3, 4, 5);
  axpy(2, PDouble(x), PDouble(y), 3);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
end;

procedure TTestVectorMath.TestAXPY_4;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4);
  y := TArray<Double>.Create(3, 4, 5, 6);
  axpy(2, PDouble(x), PDouble(y), 4);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
end;

procedure TTestVectorMath.TestAXPY_5;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5);
  y := TArray<Double>.Create(3, 4, 5, 6, 7);
  axpy(2, PDouble(x), PDouble(y), 5);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
  CheckEquals(17, y[4]);
end;

procedure TTestVectorMath.TestAXPY_6;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Double>.Create(3, 4, 5, 6, 7, 8);
  axpy(2, PDouble(x), PDouble(y), 6);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
  CheckEquals(17, y[4]);
  CheckEquals(20, y[5]);
end;

procedure TTestVectorMath.TestAXPY_3_Single;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3);
  y := TArray<Single>.Create(3, 4, 5);
  axpy(2, PSingle(x), PSingle(y), 3);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
end;

procedure TTestVectorMath.TestAXPY_4_Single;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(3, 4, 5, 6);
  axpy(2, PSingle(x), PSingle(y), 4);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
end;

procedure TTestVectorMath.TestAXPY_6_Single;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Single>.Create(3, 4, 5, 6, 7, 8);
  axpy(2, PSingle(x), PSingle(y), 6);
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
  CheckEquals(17, y[4]);
  CheckEquals(20, y[5]);
end;

{$ifdef AVX}

procedure TTestVectorMath.TestAXPY_8_Single;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  y := TArray<Single>.Create(3, 4, 5, 6, 7, 8, 9, 10);
  axpy(2, PSingle(x), PSingle(y), Length(x));
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
  CheckEquals(17, y[4]);
  CheckEquals(20, y[5]);
  CheckEquals(23, y[6]);
  CheckEquals(26, y[7]);
end;

procedure TTestVectorMath.TestAXPY_9_Single;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9);
  y := TArray<Single>.Create(3, 4, 5, 6, 7, 8, 9, 10,11);
  axpy(2, PSingle(x), PSingle(y), Length(x));
  CheckEquals(5, y[0]);
  CheckEquals(8, y[1]);
  CheckEquals(11, y[2]);
  CheckEquals(14, y[3]);
  CheckEquals(17, y[4]);
  CheckEquals(20, y[5]);
  CheckEquals(23, y[6]);
  CheckEquals(26, y[7]);
  CheckEquals(29, y[8]);
end;

{$endif}

procedure TTestVectorMath.TestAXPY_A1;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5);
  y := TArray<Double>.Create(3, 4, 5, 6, 7);
  axpy(1, PDouble(x), PDouble(y), 5);
  CheckEquals(4, y[0]);
  CheckEquals(6, y[1]);
  CheckEquals(8, y[2]);
  CheckEquals(10, y[3]);
  CheckEquals(12, y[4]);
end;

procedure TTestVectorMath.TestAXPY_1_DblCmplx;
var x, y: TArray<TCmplx128>;
    a: TCmplx128;
begin
  a.Init(1, -1);
  SetLength(x, 1);
  SetLength(y, 1);
  x[0].Init(1, 1);
  y[0].Init(3, -1);

  axpy(a, PCmplx128(x), PCmplx128(y), 1);

  CheckEquals(5, y[0].Re);
  CheckEquals(-1, y[0].Im);
end;

procedure TTestVectorMath.TestAXPY_2_DblCmplx;
var x, y: TArray<TCmplx128>;
    a: TCmplx128;
begin
  a.Init(1, -1);
  SetLength(x, 2);
  SetLength(y, 2);
  x[0].Init(1, 1);
  x[1].Init(1, 2);
  y[0].Init(3, -1);
  y[1].Init(2, -1);

  axpy(a, PCmplx128(x), PCmplx128(y), 2);

  CheckEquals(5, y[0].Re);
  CheckEquals(-1, y[0].Im);
  CheckEquals(5, y[1].Re);
  CheckEquals(0, y[1].Im);
end;

procedure TTestVectorMath.TestDot4;
var x, y: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2, 3, 4);
  y := TArray<Double>.Create(3, 4, 5, 6);
  dot(PDouble(x), PDouble(y), 4, res);
  CheckEquals(50, res);
end;

procedure TTestVectorMath.TestDot5;
var x, y: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5);
  y := TArray<Double>.Create(3, 4, 5, 6, 7);
  dot(PDouble(x), PDouble(y), 5, res);
  CheckEquals(85, res);
end;

procedure TTestVectorMath.TestDDot5_YStep2;
var x, y: TArray<Double>;
    res: Double;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5);
  y := TArray<Double>.Create(3, 0, 4, 0, 5, 0, 6, 0, 7, 0);
  res := ddot(5, PByte(x), SizeOf(Double), PByte(y), 2 * SizeOf(Double));
  CheckEquals(85, res);
end;

procedure TTestVectorMath.TestDot4_Single;
var x, y: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4);
  y := TArray<Single>.Create(3, 4, 5, 6);
  dot(PSingle(x), PSingle(y), 4, res);
  CheckEquals(50, res);
end;

procedure TTestVectorMath.TestDot6_Single;
var x, y: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6);
  y := TArray<Single>.Create(3, 4, 5, 6, 7, 8);
  dot(PSingle(x), PSingle(y), 6, res);
  CheckEquals(133, res);
end;

procedure TTestVectorMath.TestDot8_Single;
var x, y: TArray<Single>;
    res: Single;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  y := TArray<Single>.Create(3, 4, 5, 6, 7, 8, 9, 0);
  dot(PSingle(x), PSingle(y), 8, res);
  CheckEquals(196, res);
end;

procedure TTestVectorMath.TestDot2_Cmplx;
var x, y: TArray<TCmplx128>;
    res: TCmplx128;
begin
  SetLength(x, 2);
  SetLength(y, 2);
  x[0].Init(1, 2); x[1].Init(3, 4);
  y[0].Init(5, 6); y[1].Init(7, 8);
  dot(PCmplx128(x), PCmplx128(y), 2, res);
  CheckEquals(-18, res.Re, dtol);
  CheckEquals( 68, res.Im, dtol);
end;

procedure TTestVectorMath.TestSCopy_4;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  SetLength(y, 4);
  scopy(PSingle(x), PSingle(y), 4, 2, 1);
  CheckEquals(1, y[0]);
  CheckEquals(3, y[1]);
  CheckEquals(5, y[2]);
  CheckEquals(7, y[3]);
end;

procedure TTestVectorMath.TestSCopy_5;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  SetLength(y, 5);
  scopy(PSingle(x), PSingle(y), 5, 2, 1);
  CheckEquals(1, y[0]);
  CheckEquals(3, y[1]);
  CheckEquals(5, y[2]);
  CheckEquals(7, y[3]);
  CheckEquals(9, y[4]);
end;

procedure TTestVectorMath.TestSCopyConst;
var x, y: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  SetLength(y, 4);
  scopy(PSingle(x), PSingle(y), 4, 0, 1);
  CheckEquals(1, y[0]);
  CheckEquals(1, y[1]);
  CheckEquals(1, y[2]);
  CheckEquals(1, y[3]);
end;

procedure TTestVectorMath.TestSCopy_3_Double;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6);
  SetLength(y, 3);
  dcopy(PDouble(x), PDouble(y), 3, 2, 1);
  CheckEquals(1, y[0]);
  CheckEquals(3, y[1]);
  CheckEquals(5, y[2]);
end;

procedure TTestVectorMath.TestSCopy_4_Double;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  SetLength(y, 4);
  dcopy(PDouble(x), PDouble(y), 4, 2, 1);
  CheckEquals(1, y[0]);
  CheckEquals(3, y[1]);
  CheckEquals(5, y[2]);
  CheckEquals(7, y[3]);
end;

procedure TTestVectorMath.TestSCopy_5_Double;
var x, y: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  SetLength(y, 5);
  dcopy(PDouble(x), PDouble(y), 5, 2, 1);
  CheckEquals(1, y[0]);
  CheckEquals(3, y[1]);
  CheckEquals(5, y[2]);
  CheckEquals(7, y[3]);
  CheckEquals(9, y[4]);
end;

procedure TTestVectorMath.TestDiff_Double;
var x, diffs: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 4, 7, 11);
  SetLength(diffs, Length(x) - 1);
  Differences(PDouble(x), PDouble(diffs), Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
  CheckEquals(4, diffs[3]);
end;

procedure TTestVectorMath.TEstDiff3_Single;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 4, 7);
  SetLength(diffs, Length(x) - 1);
  Differences(PSingle(x), PSingle(diffs), Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
end;

procedure TTestVectorMath.TestDiff4_Single;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 4, 7, 11);
  SetLength(diffs, Length(x) - 1);
  Differences(PSingle(x), PSingle(diffs), Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
  CheckEquals(4, diffs[3]);
end;

procedure TTestVectorMath.TestDiffWithStep_Single_2;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 2, 4, 7, 11);
  SetLength(diffs, 2);
  Differences(PSingle(x), PSingle(diffs), 2, 1, Length(x));
  CheckEquals(3, diffs[0]);
  CheckEquals(7, diffs[1]);
end;

procedure TTestVectorMath.TestDiffWithStep_Single_4;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 1, 2, 2, 4, 4, 7, 7, 11, 11);
  SetLength(diffs, 4);
  Differences(PSingle(x), PSingle(diffs), 2, 1, Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
  CheckEquals(4, diffs[3]);
end;

procedure TTestVectorMath.TestDiffWithStep_Single_6;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 1, 2, 2, 4, 4, 7, 7, 11, 11, 16, 16, 22, 22);
  SetLength(diffs, 6);
  Differences(PSingle(x), PSingle(diffs), 2, 1, Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
  CheckEquals(4, diffs[3]);
  CheckEquals(5, diffs[4]);
  CheckEquals(6, diffs[5]);
end;

procedure TTestVectorMath.TestDiffWithStep_Single_8;
var x, diffs: TArray<Single>;
begin
  x := TArray<Single>.Create(1, 1, 2, 2, 4, 4, 7, 7, 11, 11, 16, 16, 22, 22, 29, 29, 37, 37);
  SetLength(diffs, 8);
  Differences(PSingle(x), PSingle(diffs), 2, 1, Length(x));
  CheckEquals(1, diffs[0]);
  CheckEquals(2, diffs[1]);
  CheckEquals(3, diffs[2]);
  CheckEquals(4, diffs[3]);
  CheckEquals(5, diffs[4]);
  CheckEquals(6, diffs[5]);
  CheckEquals(7, diffs[6]);
  CheckEquals(8, diffs[7]);
end;

procedure TTestVectorMath.TestDiffWithStep_Double;
var x, diffs: TArray<Double>;
begin
  x := TArray<Double>.Create(1, 2, 4, 7, 11);
  SetLength(diffs, 2);
  Differences(PDouble(x), PDouble(diffs), 2, 1, Length(x));
  CheckEquals(3, diffs[0]);
  CheckEquals(7, diffs[1]);
end;

procedure TTestVectorMath.TestVecNeg_Single_3;
var x, y: TArray<Single>;
    I: Integer;
begin
  x := TArray<Single>.Create(1, 2, 3);
  SetLength(y, Length(x));
  VecNeg(PSingle(x), PSingle(y), Length(x));
  for I := 0 to High(x) do
    CheckEquals(-x[I], y[I], 1e-6);
end;

procedure TTestVectorMath.TestVecNeg_Single_8;
var x, y: TArray<Single>;
    I: Integer;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8);
  SetLength(y, Length(x));
  VecNeg(PSingle(x), PSingle(y), Length(x));
  for I := 0 to High(x) do
    CheckEquals(-x[I], y[I], 1e-6);
end;

procedure TTestVectorMath.TestVecNeg_Single_9;
var x, y: TArray<Single>;
    I: Integer;
begin
  x := TArray<Single>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9);
  SetLength(y, Length(x));
  VecNeg(PSingle(x), PSingle(y), Length(x));
  for I := 0 to High(x) do
    CheckEquals(-x[I], y[I], 1e-6);
end;

procedure TTestVectorMath.TestVecAnd_3;
var x, y, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(3, 6, 12);
  y := TArray<Byte>.Create(1, 2, 4);
  SetLength(res, Length(x));
  VecAnd(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] and y[I], res[I]);
end;

procedure TTestVectorMath.TestVecAnd_4;
var x, y, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(3, 6, 12, 24);
  y := TArray<Byte>.Create(1, 2, 4, 8);
  SetLength(res, Length(x));
  VecAnd(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] and y[I], res[I]);
end;

procedure TTestVectorMath.TestVecAnd_5;
var x, y, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(3, 6, 12, 24, 48);
  y := TArray<Byte>.Create(1, 2, 4, 8, 16);
  SetLength(res, Length(x));
  VecAnd(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] and y[I], res[I]);
end;

procedure TTestVectorMath.TestVecAnd_16;
var x, y, res: TArray<Byte>;
    I: Integer;
begin                    //0  1   2   3   4   5   6   7   8  9  10  11  12  13  14  15
  x := TArray<Byte>.Create(3, 6, 12, 24, 48, 96, 48, 24, 12, 6,  3,  6, 12, 24, 48, 96);
  y := TArray<Byte>.Create(1, 2,  4,  8, 16, 32, 16,  8,  4, 2,  1,  2,  4,  8, 16, 32);
  SetLength(res, Length(x));
  VecAnd(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] and y[I], res[I]);
end;

procedure TTestVectorMath.TestVecAnd_17;
var x, y, res: TArray<Byte>;
    I: Integer;
begin                    //0  1   2   3   4   5   6   7   8  9  10  11  12  13  14  15  16
  x := TArray<Byte>.Create(3, 6, 12, 24, 48, 96, 48, 24, 12, 6,  3,  6, 12, 24, 48, 96, 48);
  y := TArray<Byte>.Create(1, 2,  4,  8, 16, 32, 16,  8,  4, 2,  1,  2,  4,  8, 16, 32, 16);
  SetLength(res, Length(x));
  VecAnd(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] and y[I], res[I]);
end;

procedure TTestVectorMath.TestVecOr_4;
var x, y, res: TArray<Byte>;
    I: Integer;
begin
  x := TArray<Byte>.Create(2, 4, 8, 16);
  y := TArray<Byte>.Create(1, 2, 4, 8);
  SetLength(res, Length(x));
  VecOr(PByte(x), PByte(y), PByte(res), Length(x));
  for I := 0 to High(res) do
    CheckEquals(x[I] or y[I], res[I]);
end;

{$endregion}

initialization

  RegisterTest(TTestVectorMath.Suite);

end.
