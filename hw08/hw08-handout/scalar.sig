signature SCALAR =
sig
  type scalar = real

  (* approximate equality *)
  val eq : scalar * scalar -> bool
      
  (* for input/output only *)
  val toString : scalar -> string

end

