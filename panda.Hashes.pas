unit panda.Hashes;

{$ifdef FPC}
  {$mode delphiunicode}{$H+}
  {$GOTO ON}
  {$Q-}
{$endif}

interface

{$ifdef FPC}
uses
    Classes
  , SysUtils
  ;
{$endif}

type
  // Noncryptographic binary hash with one-shot and incremental APIs. Buffer lengths
  // are bytes; buffers are borrowed for the duration of each call. No array dependency.
  TSpookyHash = record
  private const
    // number of uint64's in internal state
    cNumVars: Integer = 12;

    // size of the internal state
    cBlockSize: Integer = 8 * 12;// 8 * cNumVars;

    // size of buffer of unhased data in bytes
    cBufSize: Integer =  2 * 8 * 12;// 2 * cBlockSize;

	  // cConst: a constant which:
	  //  * is not zero
	  //  * is odd
	  //  * is a not-very-regular mix of 1's and 0's
	  //  * does not need any other special mathematical properties
    cConst: UInt64 = UInt64($deadbeefdeadbeef);

    ALLOW_UNALIGNED_READS: Integer = 1;

    cSz64 = SizeOf(UInt64);

  private type
    a32 = array [0..(4 * 12) (* cBuffSize div SizeOf(Cardinal) *)] of Cardinal;
    p32 = ^a32;
    a64 = array [0..(2 * 12) (* cBuffSize div SizeOf(UInt64) *)] of UInt64;
    p64 = ^a64;

  private
    fData: array[0..23 (* 2 * cNumVars - 1 *)] of UInt64;
    fState: array [0..11 (* cNumVars - 1 *)] of UInt64;
    fLength: Integer;
    fRemainder: Integer;

    // Short is used for messages under 192 bytes in length
    // Short has a low startup cost, the normal mode is good for long
    // keys, the cost crossover is at about 192 bytes.  The two modes were
    // held to the same quality bar.
    class procedure Short(
        const aMessage;       // message (array of bytes, not necessarily aligned)
        aLength: Integer;     // length of message (in bytes)
        var aHash1: UInt64;   // in/out: in the seed, out the hash value
        var aHash2: UInt64    // in/out: in the seed, out the hash value
    ); static;

    // left rotate a 64-bit value by k bytes
    class function Rot64(const x: UInt64; k: Integer): UInt64; inline; static;

    // This is used if the input is 96 bytes long or longer.
    //
    // The internal state is fully overwritten every 96 bytes.
    // Every input bit appears to cause at least 128 bits of entropy
    // before 96 other bytes are combined, when run forward or backward
    //   For every input bit,
    //   Two inputs differing in just that input bit
    //   Where "differ" means xor or subtraction
    //   And the base value is random
    //   When run forward or backwards one Mix
    // I tried 3 pairs of each; they all differed by at least 212 bits.
    class procedure Mix(const aData; var s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: UInt64); inline; static;

    // Mix all 12 inputs together so that h0, h1 are a hash of them all.
    //
    // For two inputs differing in just the input bits
    // Where "differ" means xor or subtraction
    // And the base value is random, or a counting value starting at that bit
    // The final result will have each bit of h0, h1 flip
    // For every input bit,
    // with probability 50 +- .3%
    // For every pair of input bits,
    // with probability 50 +- 3%
    //
    // This does not rely on the last Mix() call having already mixed some.
    // Two iterations was almost good enough for a 64-bit result, but a
    // 128-bit result is reported, so End() does three iterations.
    class procedure EndPartial(var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64); inline; static;

    class procedure DoEnd(var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64); inline; static;

    // The goal is for each bit of the input to expand into 128 bits of
    //   apparent entropy before it is fully overwritten.
    // n trials both set and cleared at least m bits of h0 h1 h2 h3
    //   n: 2   m: 29
    //   n: 3   m: 46
    //   n: 4   m: 57
    //   n: 5   m: 107
    //   n: 6   m: 146
    //   n: 7   m: 152
    // when run forwards or backwards
    // for all 1-bit and 2-bit diffs
    // with diffs defined by either xor or subtraction
    // with a base of all zeros plus a counter, or plus another bit, or random
    class procedure ShortMix(var h0, h1, h2, h3: UInt64); inline; static;

    // Mix all 4 inputs together so that h0, h1 are a hash of them all.
    //
    // For two inputs differing in just the input bits
    // Where "differ" means xor or subtraction
    // And the base value is random, or a counting value starting at that bit
    // The final result will have each bit of h0, h1 flip
    // For every input bit,
    // with probability 50 +- .3% (it is probably better than that)
    // For every pair of input bits,
    // with probability 50 +- .75% (the worst case is approximately that)
    class procedure ShortEnd(var h0, h1, h2, h3: UInt64); inline; static;
  public
	  // SpookyHash: hash a single message in one call, produce 128-bit output
	  class procedure Hash128(
	    const aMessage;       // message to hash
	    aLength: Integer;     // length of message in bytes
	    var aHash1: UInt64;   // in/out: in seed 1, out hash value 1
	    var aHash2: UInt64    // in/out: in seed 2, out hash value 2
    ); static;

	  // Hash64: hash a single message in one call, return 64-bit output
	  class function Hash64(
	    const aMessage;       // message to hash
	    aLength: Integer;     // length of message in bytes
	    aSeed: UInt64 = 0    // seed
    ): UInt64; static;

	  // Hash32: hash a single message in one call, produce 32-bit output
	  class function Hash32(
	    const aMessage;       // message to hash
	    aLength: Integer;     // length of message in bytes
	    aSeed: UInt64 = 0     // seed
    ): UInt32; static;

	  // Init: initialize the context of a SpookyHash
	  procedure Init(
	    aSeed1: UInt64;       // any 64-bit value will do, including 0
	    aSeed2: UInt64        // different seeds produce independent hashes
    );

	  // Update: add a piece of a message to a SpookyHash state
	  procedure Update(
	    const aMessage;       // message fragment
	    aLength: Integer      // length of message fragment in bytes
	  );

	  // Final: compute the hash for the current SpookyHash state.
	  // This does not modify the state; you can keep updating it afterward.
	  // The result is the same as if SpookyHash() had been called with
	  // all the pieces concatenated into one message.
	  procedure Final(
	    var aHash1: UInt64;   // out only: first 64 bits of hash value.
	    var aHash2: UInt64    // out only: second 64 bits of hash value.
	  );
	end;

// Reversible 32-bit integer permutation. Arithmetic wraps modulo 2^32.
function Hash_Int32(aValue: Integer): Integer;
// Inverse of Hash_Int32 for every Integer bit pattern.
function Unhash_Int32(aValue: Integer): Integer;

implementation

{$Q-}

function Hash_Int32(aValue: Integer): Integer;
var x: Cardinal absolute aValue;
begin
  x := ((x shr 16) xor x) * $45d9f3b;
  x := ((x shr 16) xor x) * $45d9f3b;
  x := (x shr 16) xor x;
  Result := Integer(x);
end;

function Unhash_Int32(aValue: Integer): Integer;
var x: Cardinal absolute aValue;
begin
  x := ((x shr 16) xor x) * $119de1f3;
  x := ((x shr 16) xor x) * $119de1f3;
  x := (x shr 16) xor x;
  Result := Integer(x);
end;

{$region 'TSpookyHash'}

class function TSpookyHash.Rot64(const x: UInt64; k: Integer): UInt64;
begin
  Result := (x shl k) or (x shr (64 - k));
end;

class procedure TSpookyHash.Mix(const aData; var s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: UInt64);
var data: array [0..11] of UInt64 absolute aData;
begin
  Inc(s0,   data[0]);    s2 :=  s2 xor s10;    s11 := s11 xor s0;     s0 := Rot64(s0,11);    Inc(s11, s1);
  Inc(s1,   data[1]);    s3 :=  s3 xor s11;    s0 :=   s0 xor s1;     s1 := Rot64(s1,32);    Inc(s0,  s2);
  Inc(s2,   data[2]);    s4 :=  s4 xor s0;     s1 :=   s1 xor s2;     s2 := Rot64(s2,43);    Inc(s1,  s3);
  Inc(s3,   data[3]);    s5 :=  s5 xor s1;     s2 :=   s2 xor s3;     s3 := Rot64(s3,31);    Inc(s2,  s4);
  Inc(s4,   data[4]);    s6 :=  s6 xor s2;     s3 :=   s3 xor s4;     s4 := Rot64(s4,17);    Inc(s3,  s5);
  Inc(s5,   data[5]);    s7 :=  s7 xor s3;     s4 :=   s4 xor s5;     s5 := Rot64(s5,28);    Inc(s4,  s6);
  Inc(s6,   data[6]);    s8 :=  s8 xor s4;     s5 :=   s5 xor s6;     s6 := Rot64(s6,39);    Inc(s5,  s7);
  Inc(s7,   data[7]);    s9 :=  s9 xor s5;     s6 :=   s6 xor s7;     s7 := Rot64(s7,57);    Inc(s6,  s8);
  Inc(s8,   data[8]);    s10 := s10 xor s6;    s7 :=   s7 xor s8;     s8 := Rot64(s8,55);    Inc(s7,  s9);
  Inc(s9,   data[9]);    s11 := s11 xor s7;    s8 :=   s8 xor s9;     s9 := Rot64(s9,54);    Inc(s8, s10);
  Inc(s10, data[10]);    s0 :=  s0 xor s8;     s9 :=   s9 xor s10;    s10 := Rot64(s10,22);  Inc(s9, s11);
  Inc(s11, data[11]);    s1 :=  s1 xor s9;     s10 := s10 xor s11;    s11 := Rot64(s11,46);  Inc(s10, s0);
end;

class procedure TSpookyHash.EndPartial(var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64);
begin
  Inc(h11, h1);    h2 := h2  xor h11;   h1 := Rot64(h1,44);
  Inc( h0, h2);    h3 := h3  xor h0;    h2 := Rot64(h2,15);
  Inc( h1, h3);    h4 := h4  xor h1;    h3 := Rot64(h3,34);
  Inc( h2, h4);    h5 := h5  xor h2;    h4 := Rot64(h4,21);
  Inc( h3, h5);    h6 := h6  xor h3;    h5 := Rot64(h5,38);
  Inc( h4, h6);    h7 := h7  xor h4;    h6 := Rot64(h6,33);
  Inc( h5, h7);    h8 := h8  xor h5;    h7 := Rot64(h7,10);
  Inc( h6, h8);    h9 := h9  xor h6;    h8 := Rot64(h8,13);
  Inc( h7, h9);    h10:= h10 xor h7;    h9 := Rot64(h9,38);
  Inc( h8, h10);   h11:= h11 xor h8;    h10:= Rot64(h10,53);
  Inc( h9, h11);   h0 := h0  xor h9;    h11:= Rot64(h11,42);
  Inc(h10, h0);    h1 := h1  xor h10;   h0 := Rot64(h0,54);
end;

class procedure TSpookyHash.DoEnd(var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64);
begin
  EndPartial(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11);
  EndPartial(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11);
  EndPartial(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11);
end;

class procedure TSpookyHash.ShortMix(var h0, h1, h2, h3: UInt64);
begin
  h2 := Rot64(h2,50);  Inc(h2, h3);  h0 := h0 xor h2;
  h3 := Rot64(h3,52);  Inc(h3, h0);  h1 := h1 xor h3;
  h0 := Rot64(h0,30);  Inc(h0, h1);  h2 := h2 xor h0;
  h1 := Rot64(h1,41);  Inc(h1, h2);  h3 := h3 xor h1;
  h2 := Rot64(h2,54);  Inc(h2, h3);  h0 := h0 xor h2;
  h3 := Rot64(h3,48);  Inc(h3, h0);  h1 := h1 xor h3;
  h0 := Rot64(h0,38);  Inc(h0, h1);  h2 := h2 xor h0;
  h1 := Rot64(h1,37);  Inc(h1, h2);  h3 := h3 xor h1;
  h2 := Rot64(h2,62);  Inc(h2, h3);  h0 := h0 xor h2;
  h3 := Rot64(h3,34);  Inc(h3, h0);  h1 := h1 xor h3;
  h0 := Rot64(h0,5);   Inc(h0, h1);  h2 := h2 xor h0;
  h1 := Rot64(h1,36);  Inc(h1, h2);  h3 := h3 xor h1;
end;

class procedure TSpookyHash.ShortEnd(var h0, h1, h2, h3: UInt64);
begin
  h3 := h3 xor h2;  h2 := Rot64(h2,15);  Inc(h3, h2);
  h0 := h0 xor h3;  h3 := Rot64(h3,52);  Inc(h0, h3);
  h1 := h1 xor h0;  h0 := Rot64(h0,26);  Inc(h1, h0);
  h2 := h2 xor h1;  h1 := Rot64(h1,51);  Inc(h2, h1);
  h3 := h3 xor h2;  h2 := Rot64(h2,28);  Inc(h3, h2);
  h0 := h0 xor h3;  h3 := Rot64(h3,9);   Inc(h0, h3);
  h1 := h1 xor h0;  h0 := Rot64(h0,47);  Inc(h1, h0);
  h2 := h2 xor h1;  h1 := Rot64(h1,54);  Inc(h2, h1);
  h3 := h3 xor h2;  h2 := Rot64(h2,32);  Inc(h3, h2);
  h0 := h0 xor h3;  h3 := Rot64(h3,25);  Inc(h0, h3);
  h1 := h1 xor h0;  h0 := Rot64(h0,63);  Inc(h1, h0);
end;

class procedure TSpookyHash.Hash128(const aMessage; aLength: Integer; var aHash1: UInt64;
  var aHash2: UInt64);
var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64;
    buf: array [0..11 (* cNumVars - 1 *)] of UInt64;
    remainder: Integer;
    p, pEnd: PByte;
begin
  if (aLength < cBufSize) then begin
    Short(aMessage, aLength, aHash1, aHash2);
    exit;
  end;

  h0 := aHash1; h3 := h0; h6 := h0; h9 := h0;
  h1 := aHash2; h4 := h1; h7 := h1; h10 := h1;
  h2 := cConst; h5 := h2; h8 := h2; h11 := h2;

  p := @aMessage;
  pEnd := p + (aLength div cBlockSize) * cBlockSize;

  // handle all whole sc_blockSize blocks of bytes
  if (ALLOW_UNALIGNED_READS = 1) or ((Byte(p) and $7) = 0) then begin
    while p < pEnd do begin
      Mix(p^, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
      Inc(PUInt64(p), cNumVars);
		end;
	end else begin
    while p < pEnd do begin
      Move(p^, buf, cBlockSize);
      Mix(buf, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
      Inc(PUInt64(p), cNumVars);
		end;
	end;

  // handle the last partial block of sc_blockSize bytes
  remainder := (aLength - (pEnd - PByte(@aMessage)));
  Move(pEnd^, buf, remainder);
  FillChar((PByte(@buf) + remainder)^, cBlockSize - remainder, 0);
  (PByte(@buf) + cBlockSize - 1)^ := remainder;
  Mix(buf, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);

  // do final mixing
  DoEnd(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
  aHash1 := h0;
  aHash2 := h1;
end;

class function TSpookyHash.Hash64(const aMessage; aLength: Integer; aSeed: UInt64): UInt64;
begin
  Result := aSeed;
  Hash128(aMessage, aLength, Result, aSeed);
end;

class function TSpookyHash.Hash32(const aMessage; aLength: Integer; aSeed: UInt64): UInt32;
var hash1, hash2: UInt64;
begin
  hash1 := aSeed;
  hash2 := aSeed;
  Hash128(aMessage, aLength, hash1, hash2);
  Result := UInt32(hash1);
end;

class procedure TSpookyHash.Short(const aMessage; aLength: Integer;
  var aHash1: UInt64; var aHash2: UInt64);
var buf: array [0..23 (* 2 * cNumVars - 1 *)] of UInt64;
    a, b, c, d: UInt64;
    remainder: NativeInt;
    p, pEnd: PByte;
label
  case_15, case_14, case_13, case_12, case_11, case_10, case_9,
  case_8, case_7, case_6, case_5, case_4, case_3, case_2,
  case_1, case_0, lbl_final;
begin
  p := @aMessage;
  if ((not ALLOW_UNALIGNED_READS) and (Byte(p) and $7)) <> 0 then begin
    Move(aMessage, buf, aLength);
    p := @buf[0];
  end;

  remainder := aLength mod 32;
  a := aHash1;
  b := aHash2;
  c := cConst;
  d := cConst;

  if (aLength > 15) then begin
    pEnd := p + ((aLength shr 5) shl 5);

    // handle all complete sets of 32 bytes
    while p < pEnd do begin
      Inc(c, PUInt64(p)^);
      Inc(p, cSz64);
      Inc(d, PUInt64(p)^);
      Inc(p, cSz64);
      ShortMix(a, b, c, d);
      Inc(a, PUint64(p)^);
      Inc(p, cSz64);
      Inc(b, PUInt64(p)^);
      Inc(p, cSz64);
    end;

    //Handle the case of 16+ remaining bytes.
    if (remainder >= 16) then begin
      Inc(c, PUInt64(p)^);
      Inc(p, cSz64);
      Inc(d, PUInt64(p)^);
      Inc(p, cSz64);
      ShortMix(a, b, c, d);
      Dec(remainder, 16);
    end;
  end;

  // Handle the last 0..15 bytes, and its length
  d := UInt64(aLength) shl 56;
  case remainder of
    15: goto case_15;
    14: goto case_14;
    13: goto case_13;
    12: goto case_12;
    11: goto case_11;
    10: goto case_10;
    9:  goto case_9;
    8:  goto case_8;
    7:  goto case_7;
    6:  goto case_6;
    5:  goto case_5;
    4:  goto case_4;
    3:  goto case_3;
    2:  goto case_2;
    1:  goto case_1;
    0:  goto case_0;
  end;

case_15:
  Inc(d, UInt64((p + 14)^) shl 48);
case_14:
  Inc(d, UInt64((p + 13)^) shl 40);
case_13:
  Inc(d, UInt64((p + 12)^) shl 32);
case_12:
  Inc(d, p32(p)^[2]);
  Inc(c, p64(p)^[0]);
  goto lbl_final;
case_11:
  Inc(d, UInt64((p + 10)^) shl 16);
case_10:
  Inc(d, UInt64((p + 9)^) shl 8);
case_9:
  Inc(d, (p + 8)^);
case_8:
  Inc(c, p64(p)^[0]);
  goto lbl_final;
case_7:
  Inc(c, UInt64((p + 6)^) shl 48);
case_6:
  Inc(c, UInt64((p + 5)^) shl 40);
case_5:
  Inc(c, UInt64((p + 4)^) shl 32);
case_4:
  Inc(c, p32(p)^[0]);
  goto lbl_final;
case_3:
  Inc(c, UInt64((p + 2)^) shl 16);
case_2:
  Inc(c, UInt64((p + 1)^) shl 8);
case_1:
  Inc(c, p^);
  goto lbl_final;
case_0:
  Inc(c, cConst);
  Inc(d, cConst);

lbl_final:
  ShortEnd(a, b, c, d);
  aHash1 := a;
  aHash2 := b;
end;

procedure TSpookyHash.Init(aSeed1: UInt64; aSeed2: uInt64);
begin
  fLength := 0;
  fRemainder := 0;
  fState[0] := aSeed1;
  fState[1] := aSeed2;
end;

procedure TSpookyHash.Update(const aMessage; aLength: Integer);
var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64;
    newLength: NativeInt;
    remainder, prefix: Byte;
    p, pEnd: PByte;
begin
  newLength := aLength + fRemainder;

  if (newLength < cBufSize) then begin
    Move(aMessage, (PByte(@fData) + fRemainder)^, aLength);
    fLength := aLength + fLength;
    fRemainder := Byte(newLength);
    exit;
  end;

  // init the variables
  if (fLength < cBufSize) then begin
    h0 := fState[0]; h3 := h0; h6 := h0; h9 := h0;
    h1 := fState[1]; h4 := h1; h7 := h1; h10 := h1;
    h2 := cConst;    h5 := h2; h8 := h2; h11 := h2;
  end else begin
    h0 := fState[0];
    h1 := fState[1];
    h2 := fState[2];
    h3 := fState[3];
    h4 := fState[4];
    h5 := fState[5];
    h6 := fState[6];
    h7 := fState[7];
    h8 := fState[8];
    h9 := fState[9];
    h10 := fState[10];
    h11 := fState[11];
  end;
  fLength := aLength + fLength;

  // if we've got anything stuffed away, use it now
  if fRemainder > 0 then begin
    prefix := cBufSize - fRemainder;
    Move(aMessage, (PByte(@fData) + fRemainder)^, prefix);
    p := @fData;
    Mix(p^, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
    Mix(p64(p)^[cNumVars], h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
    p := PByte(@aMessage) + prefix;
    Dec(aLength, prefix);
	end else
    p := @aMessage;

  // handle all whole blocks of sc_blockSize bytes
  pEnd := p + (aLength div cBlockSize) * cBlockSize;
  remainder := (aLength - (pEnd - p));
  if (ALLOW_UNALIGNED_READS or (Byte(p) and $7)) = 0 then begin
    while p < pEnd do begin
      Mix(p^, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
      Inc(PUInt64(p), cNumVars);
		end;
	end else begin
    while p < pEnd do begin
      Move(p^, fData[0], cBlockSize);
      Mix(fData, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
      Inc(PUInt64(p), cNumVars);
		end;
	end;

  fRemainder := remainder;
  Move(pEnd^, fData[0], remainder);

  // stuff away the variables
  fState[0] := h0;
  fState[1] := h1;
  fState[2] := h2;
  fState[3] := h3;
  fState[4] := h4;
  fState[5] := h5;
  fState[6] := h6;
  fState[7] := h7;
  fState[8] := h8;
  fState[9] := h9;
  fState[10] := h10;
  fState[11] := h11;
end;

procedure TSpookyHash.Final(var aHash1: UInt64; var aHash2: UInt64);
var h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11: UInt64;
    remainder: Byte;
    p: PByte;
begin
  // init variables
  if fLength < cBufSize then begin
    aHash1 := fState[0];
    aHash2 := fState[1];
    Short(fData, fLength, aHash1, aHash2);
    exit;
	end;

  p := @fData;
  remainder := fRemainder;

  h0 := fState[0];
  h1 := fState[1];
  h2 := fState[2];
  h3 := fState[3];
  h4 := fState[4];
  h5 := fState[5];
  h6 := fState[6];
  h7 := fState[7];
  h8 := fState[8];
  h9 := fState[9];
  h10 := fState[10];
  h11 := fState[11];

  if remainder >= cBlockSize then begin
    // m_data can contain two blocks; handle any whole first block
    Mix(fData, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);
    Inc(PUInt64(p), cNumVars);
    Dec(remainder, cBlockSize);
	end;

  // mix in the last partial block, and the length mod sc_blockSize
  FillChar((p + remainder)^, cBlockSize - remainder, 0);

  (p + cBlockSize - 1)^ := remainder;
  Mix(p^, h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);

  // do some final mixing
  DoEnd(h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11);

  aHash1 := h0;
  aHash2 := h1;
end;

{$endregion}

end.
