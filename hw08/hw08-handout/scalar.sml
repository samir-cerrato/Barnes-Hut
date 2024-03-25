structure Scalar : SCALAR =
struct
  type scalar = real

  val epsilon = 1E~5
      
  fun eq(x : real, y : real) = Real.abs(x - y) <= epsilon
      
  fun toString (x) =  Real.fmt (StringCvt.SCI (SOME 4)) x
end

