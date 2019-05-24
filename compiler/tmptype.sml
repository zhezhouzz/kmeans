signature TMP_TYPE_STRUCTURE =
sig
    datatype t =
             TmpVar of int
             | TmpInt
             | TmpReal
             | TmpUnit
             | TmpBool
             | TmpProduct of t * t
             | TmpArrow of t * t
             | TmpList of t
end

structure TmpTypeStructure =
struct
datatype t =
         TmpVar of int
         | TmpInt
         | TmpReal
         | TmpUnit
         | TmpBool
         | TmpProduct of t * t
         | TmpArrow of t * t
         | TmpList of t
end

signature TMP_TYPE =
sig
    structure TmpTypeStructure : TMP_TYPE_STRUCTURE
    include TMP_TYPE_STRUCTURE
    val layout : t -> string
    val eq : t * t -> bool
    val fvIn : t -> int -> bool
end

functor TmpType (TmpTypeStructure: TMP_TYPE_STRUCTURE) : TMP_TYPE =
struct
structure TmpTypeStructure = TmpTypeStructure
open TmpTypeStructure
fun layout (TmpVar i) = "T" ^ (Int.toString i)
  | layout TmpInt = "int"
  | layout TmpReal = "real"
  | layout TmpUnit = "unit"
  | layout TmpBool = "bool"
  | layout (TmpProduct (t1, t2)) = "(" ^ (layout t1) ^ " * " ^ (layout t2) ^ ")"
  | layout (TmpArrow (t1, t2)) = "(" ^ (layout t1) ^ " -> " ^ (layout t2) ^ ")"
  | layout (TmpList t) = "(" ^ (layout t) ^ " list) "

fun eq (TmpVar i, TmpVar j) = (i = j)
  | eq (TmpInt, TmpInt)  = true
  | eq (TmpReal, TmpReal)  = true
  | eq (TmpUnit, TmpUnit)  = true
  | eq (TmpBool, TmpBool)  = true
  | eq (TmpProduct (t11, t12), TmpProduct (t21, t22)) =
    (eq (t11, t21)) andalso (eq (t12, t22))
  | eq (TmpArrow (t11, t12), TmpArrow (t21, t22)) =
    (eq (t11, t21)) andalso (eq (t12, t22))
  | eq (TmpList t1, TmpList t2) = eq (t1, t2)
  | eq _ = false

fun fv (TmpVar i) = [i]
  | fv (TmpProduct (t1, t2)) = (fv t1) @ (fv t2)
  | fv (TmpArrow (t1, t2)) = (fv t1) @ (fv t2)
  | fv (TmpList t) = fv t
  | fv _ = []

fun fvIn t i = List.exists (fn e => e = i) (fv t)
end

structure TmpType = TmpType(TmpTypeStructure)
