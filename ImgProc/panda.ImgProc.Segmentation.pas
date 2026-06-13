unit panda.ImgProc.Segmentation;

interface

uses
    panda.ImgProc.Types
  , panda.ImgProc.Images
  , panda.ImgProc.CSCvt
  , panda.cvCmp
  , panda.cvMath
  , panda.cvArithmetic
  , panda.Arithmetic
  , panda.Filters.BoxFilter
  ;

type
  TThresholdMethod = (tmBinary, tmBinaryInv, tmToZero, tmToZeroInv);

procedure Threshold(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aThreshold: Byte; aMethod: TThresholdMethod = tmBinary);

procedure LocalAdaptiveBinarize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aRadius: Integer); overload;
procedure LocalAdaptiveBinarize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aRadius: Integer; aParams: array of Single); overload;

implementation

procedure Threshold(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aThreshold: Byte; aMethod: TThresholdMethod);
var I, w, h, srcWs, dstWs: NativeInt;
    pSrc, pDst: PByte;
begin
  Assert(Assigned(aSrc));

  w := aSrc.Width;
  h := aSrc.Height;

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(w, h);
  Assert((aDst.Width = w) and (aDst.Height = h));

  pSrc := aSrc.Data;
  pDst := aDst.Data;
  srcWs := aSrc.WidthStep;
  dstWs := aDst.WidthStep;

  case aMethod of
    tmBinary:
      for I := 0 to h - 1 do begin
        VecBinarize(pSrc, pDst, aThreshold, w);
        Inc(pSrc, srcWs);
        Inc(pDst, dstWs);
      end;

    tmBinaryInv:
      for I := 0 to h - 1 do begin
        VecBinarizeInv(pSrc, pDst, aThreshold, w);
        Inc(pSrc, srcWs);
        Inc(pDst, dstWs);
      end;

    tmToZero:
      for I := 0 to h - 1 do begin
        VecThreshold(pSrc, pDst, aThreshold, w);
        Inc(pSrc, srcWs);
        Inc(pDst, dstWs);
      end;

    tmToZeroInv:
      for I := 0 to h - 1 do begin
        VecThresholdInv(pSrc, pDst, aThreshold, w);
        Inc(pSrc, srcWs);
        Inc(pDst, dstWs);
      end;
  end;
end;

procedure LocalAdaptiveBinarize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aRadius: Integer);
begin
  LocalAdaptiveBinarize(aSrc, aDst, aRadius, [1, 0, 0]);
end;

procedure LocalAdaptiveBinarize(const aSrc: IImage<Byte>; var aDst: IImage<Byte>;
  aRadius: Integer; aParams: array of Single); overload;
var bf: TBoxFilter2DF32;
    srcf32, src2, t, s: IImage<Single>;
    m: TTensorF32;
    row: TArray<Single>;
    I, w, h, srcWs, dstWs, tWs, sWs: NativeInt;
    pSrc, pDst, pT, pS: PByte;
    a, b, c: Single;
begin
  Assert(Assigned(aSrc));

  w := aSrc.Width;
  h := aSrc.Height;

  if not Assigned(aDst) then
    aDst := TNDAImg<Byte>.Create(w, h);
  Assert((aDst.Width = w) and (aDst.Height = h));

  case Length(aParams) of
    0: begin
      a := 1; b := 0; c := 0;
    end;
    1: begin
      a := aParams[0]; b := 0; c := 0;
    end;
    2: begin
      a := aParams[0]; b := aParams[1]; c := 0;
    end;
  else
    a := aParams[0]; b := aParams[1]; c := aParams[2];
  end;

  bf := TBoxFilter2DF32.Create;
  try
    bf.HRadius := aRadius;
    bf.VRadius := aRadius;

    ColorConvert(aSrc, srcf32);
    pSrc := srcf32.Data;
    srcWs := srcf32.WidthStep;

    t := TNDAImg<Single>.Create(w, h);
    pT := t.Data;
    tWs := t.WidthStep;

    bf.Execute(pSrc, pT, srcf32.WidthStep, t.WidthStep, w, h);

    if (b = 0) and (c = 0) then begin
      if a = 1 then begin
        for I := 0 to h - 1 do begin
          VecBinarize(PSingle(pSrc), PSingle(pSrc), PSingle(pT), w);
          Inc(pSrc, srcWs);
          Inc(pT, tWs);
        end;
        exit;
      end;

      for I := 0 to h - 1 do begin
        VecMul(PSingle(pT), a, PSingle(pT), w);
        VecBinarize(PSingle(pSrc), PSingle(pSrc), PSingle(pT), w);
        Inc(pSrc, srcWs);
        Inc(pT, tWs);
      end;
      exit;
    end;

    if b = 0 then begin
      for I := 0 to h - 1 do begin
        VecMul(PSingle(pT), a, PSingle(pT), w);
        VecAdd(PSingle(pT), c, PSingle(pT), w);
        VecBinarize(PSingle(pSrc), PSingle(pSrc), PSingle(pT), w);
        Inc(pSrc, srcWs);
        Inc(pT, tWs);
      end;
      exit;
    end;

    m := TTensorF32(TImgUt.AsArray<Single>(srcf32));
    m := m * m;  // m <- src^2
    s := TNDAImg<Single>.Create(w, h);
    bf.Execute(m.NDA.Data, s.Data, m.NDA.Strides[0], s.WidthStep, w, h); // s <- E(src^2)
    pS := s.Data;
    sWs := s.WidthStep;
    SetLength(row, w);

    if c = 0 then begin
      for I := 0 to h - 1 do begin
        VecMul(PSingle(pT), PSingle(pT), PSingle(row), w);  // row <- t^2
        VecSub(PSingle(pS), PSingle(row), PSingle(pS), w);   // s <- E(src^2) - t^2
        cvSqrt(PSingle(pS), w);
        VecMul(PSingle(pT), a, PSingle(pT), w);
        axpy(b, PSingle(pS), PSingle(pT), w);
        VecBinarize(PSingle(pSrc), PSingle(pSrc), PSingle(pT), w);
        Inc(pSrc, srcWs);
        Inc(pT, tWs);
        Inc(pS, sWs);
      end;
    end else begin
      for I := 0 to h - 1 do begin
        VecMul(PSingle(pT), PSingle(pT), PSingle(row), w);  // row <- t^2
        VecSub(PSingle(pS), PSingle(row), PSingle(pS), w);   // s <- E(src^2) - t^2
        cvSqrt(PSingle(pS), w);
        VecMul(PSingle(pT), a, PSingle(pT), w);
        axpy(b, PSingle(pS), PSingle(pT), w);
        VecAdd(PSingle(pT), c, PSingle(pT), w);
        VecBinarize(PSingle(pSrc), PSingle(pSrc), PSingle(pT), w);
        Inc(pSrc, srcWs);
        Inc(pT, tWs);
        Inc(pS, sWs);
      end;
    end;
  finally
    ColorConvert(srcf32, aDst);
    bf.Free;
  end;
end;

end.
