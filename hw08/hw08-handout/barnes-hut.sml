structure BarnesHut =
struct
  open Mechanics
  open Plane
  open TestData


  datatype bhtree =
      Empty
    | Single of Mechanics.body
    | Cell of (Scalar.scalar * Plane.point) * BoundingBox.bbox * bhtree * bhtree * bhtree * bhtree
      (* ((mass, center), box, top-left, top-right, bottom-left, bottom-right) *)

  (* Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Empty => (0.0, Plane.origin)
        | Single (m, p, _) => (m, p)
        | Cell (com, _, _,_,_,_) => com

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *)
  fun bodyEq ((m1, p1, _) : Mechanics.body, (m2, p2, _) : Mechanics.body) : bool =
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : bhtree, t2 : bhtree) : bool =
      case (t1, t2) of
          (Empty, Empty) => true
        | (Single b1, Single b2) => bodyEq (b1, b2)
        | (Cell ((cm1, cp1), bb1, tl1,tr1,bl1,br1), Cell ((cm2, cp2), bb2, tl2,tr2,bl2,br2)) =>
              Scalar.eq (cm1, cm2) andalso
              Plane.pointEqual (cp1, cp2) andalso
              BoundingBox.equal (bb1, bb2) andalso 
              bhtreeEq (tl1,tl2) andalso bhtreeEq (tr1,tr2) andalso 
              bhtreeEq (bl1,bl2) andalso bhtreeEq (br1,br2)
        | (_, _) => false

  (* ---------------------------------------------------------------------- *)
  (* TASKS *)

  (* TASK *)
  (* Compute the barycenter of four points.
     Assumes that all points have nonnegative mass, and 
     that at least one point has strictly positive mass. *)
  fun barycenter ((m1,p1) : (Scalar.scalar * Plane.point),
                  (m2,p2) : (Scalar.scalar * Plane.point),
                  (m3,p3) : (Scalar.scalar * Plane.point),
                  (m4,p4) : (Scalar.scalar * Plane.point)) : Scalar.scalar * Plane.point =
         let val mass = (m1 + m2) + (m2 + m4)
                val mp1 = Plane.scale(Plane.vecFromPoints(Plane.origin,p1),m1)
                val mp2 = Plane.scale(Plane.vecFromPoints(Plane.origin,p2),m2)
                val mp3 = Plane.scale(Plane.vecFromPoints(Plane.origin,p3),m3)
                val mp4 = Plane.scale(Plane.vecFromPoints(Plane.origin,p4),m4)
                val n = Plane.add(Plane.add(mp1, mp2),Plane.add(mp3,mp4))
                val b = Plane.scale(n,1.0/mass)
            in (mass, Plane.head(b))
            end

  fun test_barycenter() =
      let 
          val (barymass,baryloc) =
              barycenter ((1.0,TestData.p00), (1.0,TestData.p02), (1.0,TestData.p01), (2.0,TestData.p44))
      in
          (TestData.testb "bmass" (Scalar.eq(barymass, 5.0)) true;
           TestData.testb "bloc" (Plane.pointEqual(baryloc, Plane.fromcoord(8.0/5.0, 11.0/5.0))) true)
      end

  (* TASK *)
  (* Compute the four quadrants of the bounding box *)
  fun quarters (bb : BoundingBox.bbox) : BoundingBox.bbox * BoundingBox.bbox * BoundingBox.bbox * BoundingBox.bbox =
       let val (topl,topr,botl,botr) = BoundingBox.corners(bb)
            val mid = BoundingBox.center(bb)
            val box1 = BoundingBox.from2Points (topl,mid)
            val box2 = BoundingBox.from2Points (topr,mid)
            val box3 = BoundingBox.from2Points (botl,mid)
            val box4 = BoundingBox.from2Points (botr,mid)
        in (box1,box2,box3,box4) end

  (* Test for quarters: *)
  fun test_quarters() =
      TestData.testb "q1" (let val (tl,tr,bl,br) = quarters(TestData.bb4) 
                  in BoundingBox.equal(tl,TestData.bb0) andalso BoundingBox.equal(tr,TestData.bb1) andalso
                      BoundingBox.equal(bl, TestData.bb2) andalso BoundingBox.equal(br,TestData.bb3)
                  end) true

  (* TASK *)
  (* Computes the Barnes-Hut tree for the bodies in the given sequence.
   * Assumes all the bodies are contained in the given bounding box,
     and that no two bodies have collided (or are so close that dividing the 
     bounding box will not eventually separate them).
     *)
  fun compute_tree (s : Mechanics.body Seq.seq, bb : BoundingBox.bbox) : bhtree = 
    case Seq.length(s) of
        0 => Empty
        | 1 => Single(Seq.nth(0,s))
        | _ => let val (tl, tr, bl, br) = quarters(bb)
                val tl1 = Seq.filter(fn (m, p, v) => BoundingBox.contained((false, false, false, false), p, tl), s)
                val tr1 = Seq.filter(fn (m, p, v) => BoundingBox.contained((true, false, false, false), p, tr), s)
                val bl1 = Seq.filter(fn (m, p, v) => BoundingBox.contained((false, false, true, false), p, bl), s)
                val br1 = Seq.filter(fn (m, p, v) => BoundingBox.contained((true, false, true, false), p, br), s)
                val centl = compute_tree(tl1, tl)
                val centr = compute_tree(tr1, tr)
                val cenbl = compute_tree(bl1, bl)
                val cenbr = compute_tree(br1, br)
            in Cell(barycenter(center_of_mass(centl), center_of_mass(centr),center_of_mass(cenbl), center_of_mass(cenbr)), bb, centl, centr, cenbl, cenbr)
                end

  (* Test for compute_tree: *)
  fun test_compute_tree() =
      let 
          val three_bodies = Seq.cons (TestData.body1, Seq.cons (TestData.body2, Seq.cons (TestData.body3, Seq.empty())))
          val three_bodies_tree = Cell ((3.0, TestData.p22), TestData.bb4,
                                        Cell ((2.0, TestData.p13), TestData.bb0,
                                              Single TestData.body3, Empty, Empty, Single TestData.body2), 
                                        Empty, 
                                        Empty, 
                                        Single TestData.body1)
      in
          TestData.testb "c1" (bhtreeEq (compute_tree (three_bodies, TestData.bb4), three_bodies_tree)) true
      end

  (* TASK *)
  (* too_far (p1, p2, bb, t) determines if point p1 is "too far" from 
   * a region bb with barycenter p2, given a threshold parameter t,
   * for it to be worth recuring into the region
   *)
  fun too_far (p1 : Plane.point, p2 : Plane.point, bb : BoundingBox.bbox, t : Scalar.scalar) : bool =
      let val distance = Plane.distance(p1, p2)
            val diameterdistance = (BoundingBox.diameter(bb) / distance)
      in case Real.compare(diameterdistance, t) of 
            GREATER => false    
            | _ => true 
        end

  (* TASK *)
  (* Computes the acceleration on b from the tree T using the Barnes-Hut
   * algorithm with threshold t
   *)
  fun bh_acceleration (T : bhtree, t : Scalar.scalar, b : Mechanics.body) : Plane.vec =
      case T of
          Empty => zero
            | Single(body) => Mechanics.accOn(b,body)
            | _ => let val (m1,p1,v1) = b
                        val Cell ((m2,p2),bb,t1,t2,t3,t4) = T
                  in
                  case too_far(p1,p2,bb,t) of
                       true => Mechanics.accOnPoint(p1,(m2,p2))
                    | false => (*Plane.add(Plane.adbh_acceleration(t1,t,b)) 
                                Plane.add(bh_acceleration(t2,t,b)) 
                                Plane.add(bh_acceleration(t3,t,b))
                                Plane.add(bh_acceleration(t4,t,b))*)

                               ( Plane.add(Plane.add(bh_acceleration(t1,t,b)
                                    ,
                                    bh_acceleration(t2,t,b)
                                ), 
                                Plane.add(bh_acceleration(t3, t, b)
                                ,
                                (bh_acceleration(t4, t,b)))
                    )
                    )
                  end

  (* TASK
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)
  fun barnes_hut (threshold : Scalar.scalar, s : Mechanics.body Seq.seq) : Plane.vec Seq.seq = 
      Seq.map(fn b => bh_acceleration(compute_tree(s,BoundingBox.fromPoints(Seq.map(fn (m, p, v) => p, s))), threshold, b), s)


  (* Default value of the threshold, theta = 0.5 *)
  val threshold = 1.0/2.0

  fun accelerations(bodies : Mechanics.body Seq.seq) : Plane.vec Seq.seq = barnes_hut(threshold, bodies)

end
