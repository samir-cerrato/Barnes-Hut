
structure Plane : SPACE =
struct

  type scalar = Scalar.scalar

  datatype coord = Coord of scalar * scalar
  datatype vector = Vec of scalar * scalar

  type point = coord
  type vec = vector

  (* add(v1, v2) evaluates to the sum of the vectors *)
  fun add (Vec (x1,y1), Vec (x2,y2)) : vec =
      Vec (x1 + x2, y1 + y2)

  (* scale(v, c) evaluates to the scalar product of v with c *)
  fun scale(Vec (x,y) , c : scalar): vec =
      Vec (x *c , y * c)

  (* vecFromPoints(X,Y) is the vector from X to Y *)
  fun vecFromPoints(Coord (x1, y1) : point, Coord (x2, y2) : point) : vec =
      Vec (x2 - x1, y2 - y1)

  (* The origin point *)
  val origin : point = Coord (0.0, 0.0)

  (* Computes the cartesian coordinates of the given point *)
  fun cartcoord (Coord (x, y) : point) : scalar * scalar = (x,y)

  (* Return a point in 2D space with the given Cartesian coordinates *)
  fun fromcoord ((x, y) : scalar * scalar) = Coord (x, y)

  (* Computes the distance between the argument points *)
  fun distance (Coord (x1,y1), Coord (x2,y2)) =
      let
        val (dx,dy) = (x2 - x1, y2 - y1)
      in
          (Math.sqrt (dx * dx + dy * dy))
      end

  (* Computes the magnitude of the given vector *)
  fun mag (Vec (x, y) : vec) : scalar = distance (Coord (x, y), origin)

  fun vecToString (Vec (x,y) : vec) =
     "(" ^ Scalar.toString x ^ ", " ^ Scalar.toString y ^ ")"

  fun pointToString (Coord (x,y) : point) =
    "(" ^ Scalar.toString x ^ ", " ^ Scalar.toString y ^ ")"

  (* Tests two points for equality *)
  fun pointEqual (Coord (x1, y1) : point, Coord (x2, y2) : point) : bool =
    Scalar.eq(x1,x2) andalso Scalar.eq(y1,y2)

  fun displace (Coord (x,y) : point, Vec (v1, v2) : vec) : point =
      Coord (x + v1, y + v2)

  val zero : vec = Vec (0.0,0.0)

  fun unitVec (V as Vec v : vec) : vec = scale(V, 1.0 / (mag V))

  fun sum (f : 'a -> vec, s : 'a Seq.seq) : vec = Seq.mapreduce (f, zero, add, s)

  (* compute the mid-point of a line that connects two points *)
  fun midpoint (Coord (x1, y1) : point, Coord (x2, y2) : point) : point =
      Coord ((x1+ x2) /2.0,
             (y1+ y2) / 2.0)

  (* Compute the point corresponding to the dispacement by the given vector
   * from the origin
   *)
  fun head (v : vec) : point = displace (origin, v)
end
