structure TestData : sig 
                         (* some points and bounding boxes and bodies to use for testing *)
                         val p00 : Plane.point 
                         val p44 : Plane.point 
                         val p02 : Plane.point 
                         val p24 : Plane.point 
                         val p22 : Plane.point 
                         val p20 : Plane.point 
                         val p42 : Plane.point 
                         val p01 : Plane.point 
                         val p11 : Plane.point 
                         val p40 : Plane.point 
                         val p04 : Plane.point 
                         val p13 : Plane.point 
                         val p33 : Plane.point 

                         val bb0 : BoundingBox.bbox
                         val bb1 : BoundingBox.bbox
                         val bb2 : BoundingBox.bbox
                         val bb3 : BoundingBox.bbox
                         val bb4 : BoundingBox.bbox

                         val body1 : Mechanics.body
                         val body2 : Mechanics.body
                         val body3 : Mechanics.body

                         val sun : Mechanics.body
                         val mercury : Mechanics.body
                         val venus : Mechanics.body
                         val earth : Mechanics.body
                         val mars : Mechanics.body
                         val jupiter : Mechanics.body
                         val saturn : Mechanics.body
                         val uranus : Mechanics.body
                         val neptune : Mechanics.body
                           
                         val one_body : Mechanics.body Seq.seq
                         val two_body : Mechanics.body Seq.seq
                         val solar_system : Mechanics.body Seq.seq

                         val testb : string -> bool -> bool -> unit
                   end
=
struct
  structure BB = BoundingBox

  (* some points and bounding boxes and bodies to use for testing *)
  val p00 = Plane.origin
  val p44 = Plane.fromcoord (4.0, 4.0)
  val p02 = Plane.fromcoord (0.0, 2.0)
  val p24 = Plane.fromcoord (2.0, 4.0)
  val p22 = Plane.fromcoord (2.0, 2.0)
  val p20 = Plane.fromcoord (2.0, 0.0)
  val p42 = Plane.fromcoord (4.0, 2.0)
  val p01 = Plane.fromcoord (0.0, 1.0)
  val p11 = Plane.fromcoord (1.0, 1.0)
  val p40 = Plane.fromcoord (4.0, 0.0)
  val p04 = Plane.fromcoord (0.0, 4.0)
  val p13 = Plane.fromcoord (1.0, 3.0)
  val p33 = Plane.fromcoord (3.0, 3.0)

  val bb0 : BB.bbox = BB.from2Points (p02,p24)
  val bb1 : BB.bbox = BB.from2Points (p22,p44)
  val bb2 : BB.bbox = BB.from2Points (p00,p22)
  val bb3 : BB.bbox = BB.from2Points (p20,p42)
  val bb4 : BB.bbox = BB.from2Points (p00,p44)

  val body1 : Mechanics.body = (1.0, p40, Plane.zero)
  val body2 : Mechanics.body = (1.0, p22, Plane.zero)
  val body3 : Mechanics.body = (1.0, p04, Plane.zero)


  fun b (mass : real) (dist : real) (vel : real) =
      (mass,
       Plane.fromcoord (0.0, dist),
       Plane.vecFromPoints (Plane.origin, Plane.fromcoord (vel, 0.0)))

  (* astronomical constants sourced from Wikipedia, the font of all knowledge *)

  val sun = b (1.98892E30) 0.0 0.0              (* sun *)
  val mercury = b (3.3022E23) (5.79091E10) (4.7870E4)  (* mercury *)
  val venus = b (4.8685E24) (~1.0820893E11) (~3.5020E4)    (* venus *)
  val earth = b (5.9736E24) (~1.4960E11) (~2.9780E4)    (* earth *)
  val mars = b (6.4185E23) (2.279391E11) (2.4077E4)     (* mars *)
  val jupiter = b (1.8986E27) (7.785472E11)  (1.3070E4)  (* jupiter *)
  val saturn = b (5.6846E26) (~1.43344937E12) (~9.69E3)   (* saturn *)
  val uranus = b (8.681E25)  (2.87667908E12)  (6.81E3)    (* uranus *)
  val neptune = b (1.0243E26) (~4.50344366E12) (~5.43E3)  (* neptune *)

  val one_body = Seq.singleton sun
  val two_body = Seq.cons (sun ,Seq.cons (earth, Seq.empty()))
  val solar_system = 
      List.foldr (fn (x,y) => Seq.cons (x, y)) (Seq.empty())
      [sun, mercury, venus, earth, mars, jupiter, saturn, uranus, neptune]



      
  fun testb (s : string) (n : bool) (m : bool) : unit =
      case n = m of
          true => print ("Test " ^ s ^ " OK\n")
        | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

end
