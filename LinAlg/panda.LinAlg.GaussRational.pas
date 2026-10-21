unit panda.LinAlg.GaussRational;

interface

uses
    panda.NumsAP
  , panda.LinAlg.Gauss
  , panda.LinAlg.Base
  , panda.LinAlg.NumericRational
  ;

type
  /// <summary>
  ///   General Gaussian elimination for TRational, including rectangular and
  ///   rank-deficient systems. Returns a particular solution and null-space basis.
  ///   Init copies inputs; InitWithoutCopy aliases writable row-contiguous storage.
  ///   Solve consumes initialization; results retain independent storage.
  ///   Arithmetic and allocation exceptions propagate.
  /// </summary>
  /// <remarks>
  ///   Every input, including zero, must be an explicitly constructed valid
  ///   rational. Default(TRational) is not a mathematical zero and is not
  ///   validated. Pivots use the first exact nonzero candidate; no tolerances.
  /// </remarks>
  TGaussElimRational = class(TGaussElim<TRational>)
  protected
    function CreateNumeric: TLinAlgNumeric<TRational>; override;
  end;

implementation

function TGaussElimRational.CreateNumeric: TLinAlgNumeric<TRational>;
begin
  Result := TLinAlgNumericRational.Create;
end;

end.
