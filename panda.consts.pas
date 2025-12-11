unit panda.Consts;

interface

const
  cI32Sz  = SizeOf(Integer);
  cI64Sz  = SizeOf(Int64);
  cNISz   = SizeOf(NativeInt);
  cF32Sz  = SizeOf(Single);
  cF64Sz  = SizeOf(Double);

resourcestring
  csSameArrays = 'Source and Destination arrays must not be the same.';
  csArgumentOutOfRange = 'Argument out of range.';
  csUnbalancedOperation = 'Unbalanced stack or queue operation.';

  csInvBroadcastErr = 'Could not broadcast input array from shape %s into shape %s.';
  csInvArrayElem    = 'Setting an array element with a sequence.';
  csInvReshape      = 'Cannot reshape array of size %d into shape %s.';
  csInvIdxForScalar = 'Invalid index to scalar varibale.';
  csNotWriteable    = 'Array is not writeable.';
  csInvCastToVec    = 'Array with shape %s cannot be converted to vector.';
  csCContArrExp     = 'C contiguous array is expected.';
  csBroadcastErr    = 'Operands cannot be broadcast together with shapes %s %s.';
  csBroadcastToErr  = 'Cannot broadcast input array from shape %s into shape %s.';
  csTooManyIndices  = 'Too many indices for array: array is %d-dimensional, but %d were indexed.';

implementation

end.

