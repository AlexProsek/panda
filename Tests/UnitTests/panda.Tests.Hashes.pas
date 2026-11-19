unit panda.Tests.Hashes;

interface

uses
    SysUtils
  , Math
{$ifndef FPC}
  , TestFramework
{$else}
  , fpcunit
  , testutils
  , testregistry
{$endif}
  , panda.Hashes
  , panda.NumsAP
  , panda.NumsLowLvl
  ;

type
  TSpookyHashTests = class(TTestCase)
  protected const
    cHelloWorld: String = 'hello world';
  protected
    fSH: TSpookyHash;
    function Hash128ToString(const aHash1, aHash2: UInt64): String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResults;
    procedure TestResults_ByChunks;
    procedure IntegerRoundTrip;
  end;

implementation

{$R-}

{$region 'TSpookyHashTests'}

function TSpookyHashTests.Hash128ToString(const aHash1, aHash2: UInt64): String;
var data: TLimbArray;
    i: TInteger;
begin
  SetLength(data, 2 * SizeOf(UInt64) div SizeOf(TLimb));
  Move(aHash1, data[0], SizeOf(UInt64));
  Move(aHash2, data[SizeOf(UInt64) div SizeOf(TLimb)], SizeOf(UInt64));
  i := data;
  Result := i.ToDecimalString;
end;

procedure TSpookyHashTests.SetUp;
begin
  inherited;
  fSH.Init(0, 0);
end;

procedure TSpookyHashTests.TearDown;
begin
  inherited;
end;

procedure TSpookyHashTests.TestResults;
const
  cBuffSize = 512;
  expected: array [0..511] of UInt32 = (
	  $a24295ec, $fe3a05ce, $257fd8ef, $3acd5217,
	  $fdccf85c, $c7b5f143, $3b0c3ff0, $5220f13c,
	  $a6426724, $4d5426b4, $43e76b26, $051bc437,
	  $d8f28a02, $23ccc30e, $811d1a2d, $039128d4,
	  $9cd96a73, $216e6a8d, $97293fe8, $e4fc6d09,
	  $1ad34423, $9722d7e4, $5a6fdeca, $3c94a7e1,
	  $81a9a876, $ae3f7c0e, $624b50ee, $875e5771,
	  $0095ab74, $1a7333fb, $056a4221, $a38351fa,

	  $73f575f1, $8fded05b, $9097138f, $bd74620c,
	  $62d3f5f2, $07b78bd0, $bafdd81e, $0638f2ff,
	  $1f6e3aeb, $a7786473, $71700e1d, $6b4625ab,
	  $f02867e1, $b2b2408f, $9ce21ce5, $a62baaaf,
	  $26720461, $434813ee, $33bc0f14, $aaab098a,
	  $750af488, $c31bf476, $9cecbf26, $94793cf3,
	  $e1a27584, $e80c4880, $1299f748, $25e55ed2,
	  $405e3feb, $109e2412, $3e55f94f, $59575864,

	  $365c869d, $c9852e6a, $12c30c62, $47f5b286,
	  $b47e488d, $a6667571, $78220d67, $a49e30b9,
	  $2005ef88, $f6d3816d, $6926834b, $e6116805,
	  $694777aa, $464af25b, $0e0e2d27, $0ea92eae,
	  $602c2ca9, $1d1d79c5, $6364f280, $939ee1a4,
	  $3b851bd8, $5bb6f19f, $80b9ed54, $3496a9f1,
	  $df815033, $91612339, $14c516d6, $a3f0a804,
	  $5e78e975, $f408bcd9, $63d525ed, $a1e459c3,

	  $fde303af, $049fc17f, $e7ed4489, $faeefdb6,
	  $2b1b2fa8, $c67579a6, $5505882e, $e3e1c7cb,
	  $ed53bf30, $9e628351, $8fa12113, $7500c30f,
	  $de1bee00, $f1fefe06, $dc759c00, $4c75e5ab,
	  $f889b069, $695bf8ae, $47d6600f, $d2a84f87,
	  $a0ca82a9, $8d2b750c, $e03d8cd7, $581fea33,
	  $969b0460, $36c7b7de, $74b3fd20, $2bb8bde6,
	  $13b20dec, $a2dcee89, $ca36229d, $06fdb74e,


	  $6d9a982d, $02503496, $bdb4e0d9, $bd1f94cf,
	  $6d26f82d, $cf5e41cd, $88b67b65, $3e1b3ee4,
	  $b20e5e53, $1d9be438, $cef9c692, $299bd1b2,
	  $b1279627, $210b5f3d, $5569bd88, $9652ed43,
	  $7e8e0f8c, $dfa01085, $cd6d6343, $b8739826,
	  $a52ce9a0, $d33ef231, $1b4d92c2, $abfa116d,
	  $cdf47800, $3a4eefdc, $d01f3bcf, $30a32f46,
	  $fb54d851, $06a98f67, $bdcd0a71, $21a00949,

	  $fe7049c9, $67ef46d2, $a1fabcbc, $a4c72db4,
	  $4a8a910d, $85a890ad, $c37e9454, $fc3d034a,
	  $6f46cc52, $742be7a8, $e94ecbc5, $5f993659,
	  $98270309, $8d1adae9, $ea6e035e, $293d5fae,
	  $669955b3, $5afe23b5, $4c74efbf, $98106505,
	  $fbe09627, $3c00e8df, $5b03975d, $78edc83c,
	  $117c49c6, $66cdfc73, $fa55c94f, $5bf285fe,
	  $2db49b7d, $fbfeb8f0, $b7631bab, $837849f3,

	  $f77f3ae5, $6e5db9bc, $fdd76f15, $545abf92,
	  $8b538102, $dd5c9b65, $a5adfd55, $ecbd7bc5,
	  $9f99ebdd, $67500dcb, $f5246d1f, $2b0c061c,
	  $927a3747, $c77ba267, $6da9f855, $6240d41a,
	  $e9d1701d, $c69f0c55, $2c2c37cf, $12d82191,
	  $47be40d3, $165b35cd, $b7db42e1, $358786e4,
	  $84b8fc4e, $92f57c28, $f9c8bbd7, $ab95a33d,
	  $11009238, $e9770420, $d6967e2a, $97c1589f,

	  $2ee7e7d3, $32cc86da, $e47767d1, $73e9b61e,
	  $d35bac45, $835a62bb, $5d9217b0, $43f3f0ed,
	  $8a97911e, $4ec7eb55, $4b5a988c, $b9056683,
	  $45456f97, $1669fe44, $afb861b8, $8e83a19c,
	  $0bab08d6, $e6a145a9, $c31e5fc2, $27621f4c,
	  $795692fa, $b5e33ab9, $1bc786b6, $45d1c106,
	  $986531c9, $40c9a0ec, $ff0fdf84, $a7359a42,
	  $fd1c2091, $f73463d4, $51b0d635, $1d602fb4,


	  $c56b69b7, $6909d3f7, $a04d68f4, $8d1001a7,
	  $8ecace50, $21ec4765, $3530f6b0, $645f3644,
	  $9963ef1e, $2b3c70d5, $a20c823b, $8d26dcae,
	  $05214e0c, $1993896d, $62085a35, $7b620b67,
	  $1dd85da2, $09ce9b1d, $d7873326, $063ff730,
	  $f4ff3c14, $09a49d69, $532062ba, $03ba7729,
	  $bd9a86cc, $e26d02a7, $7ccbe5d3, $4f662214,
	  $8b999a66, $3d0b92b4, $70b210f0, $f5b8f16f,

	  $32146d34, $430b92bf, $8ab6204c, $35e6e1ff,
	  $c2f6c2fa, $a2df8a1a, $887413ec, $7cb7a69f,
	  $7ac6dbe6, $9102d1cb, $8892a590, $c804fe3a,
	  $dfc4920a, $fc829840, $8910d2eb, $38a210fd,
	  $9d840cc9, $7b9c827f, $3444ca0c, $071735ab,
	  $5e9088e4, $c995d60e, $be0bb942, $17b089ae,
	  $050e1054, $cf4324f7, $1e3e64dd, $436414bb,
	  $c48fc2e3, $6b6b83d4, $9f6558ac, $781b22c5,

	  $7147cfe2, $3c221b4d, $a5602765, $8f01a4f0,
	  $2a9f14ae, $12158cb8, $28177c50, $1091a165,
	  $39e4e4be, $3e451b7a, $d965419c, $52053005,
	  $0798aa53, $e6773e13, $1207f671, $d2ef998b,
	  $ab88a38f, $c77a8482, $a88fb031, $5199e0cd,
	  $01b30536, $46eeb0ef, $814259ff, $9789a8cf,
	  $376ec5ac, $7087034a, $948b6bdd, $4281e628,
	  $2c848370, $d76ce66a, $e9b6959e, $24321a8e,

	  $deddd622, $b890f960, $ea26c00a, $55e7d8b2,
	  $eab67f09, $9227fb08, $eebbed06, $cac1b0d1,
	  $b6412083, $05d2b0e7, $9037624a, $c9702198,
	  $2c8d1a86, $3e7d416e, $c3f1a39f, $f04bdce4,
	  $c88cdb61, $bdc89587, $4d29b63b, $6f24c267,
	  $4b529c87, $573f5a53, $db3316e9, $288eb53b,
	  $d2c074bd, $ef44a99a, $2b404d2d, $f6706464,
	  $fe824f4c, $c3debaf8, $12f44f98, $03135e76,


	  $b4888e7f, $b6b2325d, $3a138259, $513c83ec,
	  $2386d214, $94555500, $fbd1522d, $da2af018,
	  $15b054c0, $5ad654e6, $b6ed00aa, $a2f2180e,
	  $5f662825, $ecd11366, $1de5e99d, $07afd2ad,
	  $cf457b04, $e631e10b, $83ae8a21, $709f0d59,
	  $3e278bf9, $246816db, $9f5e8fd3, $c5b5b5a2,
	  $d54a9d5c, $4b6f2856, $2eb5a666, $fc68bdd4,
	  $1ed1a7f8, $98a34b75, $c895ada9, $2907cc69,

	  $87b0b455, $ddaf96d9, $e7da15a6, $9298c82a,
	  $72bd5cab, $2e2a6ad4, $7f4b6bb8, $525225fe,
	  $985abe90, $ac1fd6e1, $b8340f23, $92985159,
	  $7d29501d, $e75dc744, $687501b4, $92077dc3,
	  $58281a67, $e7e8e9be, $d0e64fd1, $b2eb0a30,
	  $0e1feccd, $c0dc4a9e, $5c4aeace, $2ca5b93c,
	  $ee0ec34f, $ad78467b, $0830e76e, $0df63f8b,
	  $2c2dfd95, $9b41ed31, $9ff4cddc, $1590c412,

	  $2366fc82, $7a83294f, $9336c4de, $2343823c,
	  $5b681096, $f320e4c2, $c22b70e2, $b5fbfb2a,
	  $3ebc2fed, $11af07bd, $429a08c5, $42bee387,
	  $58629e33, $fb63b486, $52135fbe, $f1380e60,
	  $6355de87, $2f0bb19a, $167f63ac, $507224cf,
	  $f7c99d00, $71646f50, $74feb1ca, $5f9abfdd,
	  $278f7d68, $70120cd7, $4281b0f2, $dc8ebe5c,
	  $36c32163, $2da1e884, $61877598, $bef04402,

	  $304db695, $fa8e9add, $503bac31, $0fe04722,
	  $f0d59f47, $cdc5c595, $918c39dd, $0cad8d05,
	  $6b3ed1eb, $4d43e089, $7ab051f8, $deec371f,
	  $0f4816ae, $f8a1a240, $d15317f6, $b8efbf0b,
	  $cdd05df8, $4fd5633e, $7cf19668, $25d8f422,
	  $72d156f2, $2a778502, $da7aefb9, $4f4f66e8,
	  $19db6bff, $74e468da, $a754f358, $7339ec50,
	  $139006f6, $efbd0b91, $217e9a73, $939bd79c
);
var buf: array [0..511] of Byte;
    saw: array [0..512] of UInt32;
    I: Integer;
begin
  for I := 0 to cBuffSize - 1 do begin
    buf[I] := I + 128;
    saw[I] := fSH.Hash32(buf, I, 0);
    if saw[I] <> expected[I] then
      Fail(Format('%d: saw 0x%.8x, expected 0x%.8x', [i, saw[i], expected[i]]));
    ;
  end;
end;

procedure TSpookyHashTests.TestResults_ByChunks;
var buf: array [0..511] of Byte;
    h1, h2: UInt64;
    h32, expected: Cardinal;
    I, count, chSize, chCnt, pos, rest: Integer;
begin
  for I := 0 to 511 do
    buf[I] := I + 128;
  expected := fSH.Hash32(buf, 511);

  fSH.Init(0, 0);
  try
    chSize := 32;
    count := 511;
    pos := 0;
    rest := count;
    while pos < count do begin
      chCnt := Min(rest, chSize);
      fSH.Update(buf[pos], chCnt);
      Dec(rest, chCnt);
      Inc(pos, chCnt);
    end;

  finally
    fSH.Final(h1, h2);
    h32 := Cardinal(h1);
    CheckEquals(expected, h32);
  end;
end;

procedure TSpookyHashTests.IntegerRoundTrip;
const values: array [0..6] of Integer = (0, 1, -1, Low(Integer), High(Integer), 123456789, -987654321);
var value: Integer;
begin
  for value in values do begin
    CheckEquals(value, Unhash_Int32(Hash_Int32(value)));
    CheckEquals(value, Hash_Int32(Unhash_Int32(value)));
  end;
end;

{$endregion}

initialization

RegisterTest(TSpookyHashTests.Suite);

end.
