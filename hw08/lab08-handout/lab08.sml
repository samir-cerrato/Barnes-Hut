
val true = CM.make("sources.cm");

fun ilToString(l : int list) : string =
    case l of
        [] => "[]"
      | x :: xs => Int.toString x ^ "::" ^ ilToString(xs)

fun illToString(l : int list list) : string =
    case l of
        [] => "[]"
      | x :: xs => "(" ^ ilToString x ^ ")::" ^ illToString(xs)

fun silToString(l : (string * int) list) : string =
    case l of
        [] => "[]"
      | (x,y) :: xs => "(" ^ x ^ "," ^ Int.toString y ^ ")::" ^ silToString(xs)

fun testi (s : string) (n : int) (m : int) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Int.toString m ^ "\n    Got: " ^ Int.toString n ^ "\n")

fun testil (s : string) (n : int list) (m : int list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ ilToString m ^ "\n    Got: " ^ ilToString n ^ "\n")

fun testsil (s : string) (n : (string * int) list) (m : (string * int) list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ silToString m ^ "\n    Got: " ^ silToString n ^ "\n")

            
fun testill (s : string) (n : int list list) (m : int list list) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ illToString m ^ "\n    Got: " ^ illToString n ^ "\n")

fun testb (s : string) (n : bool) (m : bool) : unit =
    case n = m of
        true => print ("Test " ^ s ^ " OK\n")
      | false => print ("Test " ^ s ^ " FAILED:\n    Expected: " ^ Bool.toString m ^ "\n    Got: " ^ Bool.toString n  ^ "\n")

(* ---------------------------------------------------------------------- *)
    
exception Unimplemented
val map = Seq.map

fun inteq(x,y) = case Int.compare (x,y) of EQUAL => true | _ => false
fun stringeq(x,y) = case String.compare (x,y) of EQUAL => true | _ => false

fun oddP (n : int) = inteq(n mod 2, 1)

fun seqFromList2 (l : 'a list list) : 'a Seq.seq Seq.seq = Seq.fromlist (List.map Seq.fromlist l)
fun seqToList2 (l : 'a Seq.seq Seq.seq) : 'a list list = Seq.tolist (Seq.map (Seq.tolist, l))

(* ---------------------------------------------------------------------- *)

(* Task *)

fun eligible (l : (string * int) Seq.seq) : (string * int) Seq.seq = raise Unimplemented

fun test_eligible () =
    (testsil "a1" (Seq.tolist (eligible (Seq.fromlist [("Dan",1982),("SJ",2019)]))) [("Dan",40)])

    
(* ---------------------------------------------------------------------- *)

(* Task *)

fun seqExists (f : 'a -> bool, s : 'a Seq.seq) : bool = raise Unimplemented

fun test_exists() =
    (testb "e1" (seqExists (oddP , Seq.fromlist [1,2,3])) true;
     testb "e2" (seqExists (oddP , Seq.fromlist [2,4,6])) false)
    
(* ---------------------------------------------------------------------- *)

(* Task *)

(* Task *)
fun increasing (n : int) : int Seq.seq = raise Unimplemented

fun test_increasing() = 
    (testil "i1" (Seq.tolist (increasing 4)) [0,1,2,3])
    
fun reverse (s : 'a Seq.seq) : 'a Seq.seq = raise Unimplemented

fun test_reverse() = 
    (testil "r1" (Seq.tolist (reverse (Seq.fromlist [1,2,3,4]))) [4,3,2,1])

fun myAppend (s1 : 'a Seq.seq, s2 : 'a Seq.seq) : 'a Seq.seq = raise Unimplemented

fun test_append() = 
    (testil "a1" (Seq.tolist (myAppend (Seq.fromlist [1,2,3,4], Seq.fromlist [5,6,7,8]) ))
                [1,2,3,4,5,6,7,8])

(* Task *)

(* assumes s is valid: 
   rectangular n x m where n,m > 0
   *)
fun transpose (s : 'a Seq.seq Seq.seq) : 'a Seq.seq Seq.seq = raise Unimplemented

fun test_transpose() = 
    (testill "t1" (seqToList2 (transpose (seqFromList2 [[1,2,3],[4,5,6]]))) [[1,4],[2,5],[3,6]])

(* ---------------------------------------------------------------------- *)

fun run() =
    (test_exists();
     test_increasing();
     test_reverse();
     test_append();
     test_transpose())