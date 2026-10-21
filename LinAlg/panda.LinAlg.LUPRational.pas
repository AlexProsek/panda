unit panda.LinAlg.LUPRational;

interface

uses
    panda.NumsAP
  , panda.LinAlg.LUP
  , panda.LinAlg.Base
  , panda.LinAlg.NumericRational
  ;

type
  /// <summary>
  ///   Ordinary or blocked LUP initialization and repeated cached operations for
  ///   TRational. Uses the shared TLinAlgNumericRational numerical policy.
  ///   Inputs and intermediate values must be valid for the scalar arithmetic;
  ///   they are not scanned for validity. Default(TRational) is not zero;
  ///   construct every input explicitly. Pivots use the first exact nonzero
  ///   candidate. Arithmetic exceptions propagate.
  /// </summary>
  TLUPDecompositionRational = class(TLUPDecomposition<TRational>)
  protected
    function CreateNumeric: TLinAlgNumeric<TRational>; override;
  end;

implementation

function TLUPDecompositionRational.CreateNumeric: TLinAlgNumeric<TRational>;
begin
  Result := TLinAlgNumericRational.Create;
end;

end.
