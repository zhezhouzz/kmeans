signature APPRABLE =
sig
    type t
    val linearAppr : int -> (int * t) -> (int * t) -> t
    val toString : t -> string
end

structure IntAppr : APPRABLE=
struct
type t = int
fun linearAppr idx (idx1, r1) (idx2, r2) =
    ((idx - idx1) * r2 + (idx2 - idx) * r1) div (idx2 - idx1)
val toString = Int.toString
end

structure RealAppr : APPRABLE =
struct
type t = real
fun linearAppr idx (idx1, r1) (idx2, r2) =
    (((Real.fromInt (idx - idx1)) * r2 +
      (Real.fromInt (idx2 - idx)) * r1
     ) / (Real.fromInt (idx2 - idx1)))
val toString = Real.toString
end

functor PairAppr (structure A: APPRABLE
                  structure B: APPRABLE) : APPRABLE =
struct
type t = A.t * B.t
fun linearAppr idx (idx1, (a1, b1)) (idx2, (a2, b2)) =
    let
        val a = A.linearAppr idx (idx1, a1) (idx2, a2)
        val b = B.linearAppr idx (idx1, b1) (idx2, b2)
    in
        (a, b)
    end
fun toString (x, y) = "(" ^ (A.toString x) ^ ", " ^ (B.toString y) ^ ")"
end

functor TripleAppr (structure A: APPRABLE
                    structure B: APPRABLE
                    structure C: APPRABLE) : APPRABLE =
struct
type t = A.t * B.t * C.t
fun linearAppr idx (idx1, (a1, b1, c1)) (idx2, (a2, b2, c2)) =
    let
        val a = A.linearAppr idx (idx1, a1) (idx2, a2)
        val b = B.linearAppr idx (idx1, b1) (idx2, b2)
        val c = C.linearAppr idx (idx1, c1) (idx2, c2)
    in
        (a, b, c)
    end
fun toString (x, y, z) = "(" ^ (A.toString x) ^ ", " ^ (B.toString y) ^ ", " ^ (C.toString z) ^ ")"
end
