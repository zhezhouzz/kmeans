signature TYPED_AST =
sig
    structure Atoms: ATOMS

    datatype tmptype =
             TmpVar of int
             | TmpInt
             | TmpReal
             | TmpUnit
             | TmpProduct of tmptype * tmptype
             | TmpArrow of tmptype * tmptype
             | TmpList of tmptype

    datatype exp =
             Var of tmptype * Atoms.Id.t
             | Pair of tmptype * exp * exp
             | Fst of tmptype * exp
             | Snd of tmptype * exp
             | Ifte of tmptype * exp * exp * exp
             | App of tmptype * exp * exp
             | Abs of tmptype * Atoms.Id.t * Atoms.Type.t * exp
             | Con of tmptype * Atoms.Const.t
             | Op of tmptype * Atoms.Operator.t * exp * exp
             | Map of tmptype * exp * exp
             | Foldl of tmptype * exp * exp * exp
             | Mapi of tmptype * exp * exp
             | Foldli of tmptype * exp * exp * exp
             | Nth of tmptype * exp
             | Loop of tmptype * exp * exp * exp
             | Unit of tmptype
    type top_level = exp
    val tmptypeLayout : tmptype -> string
    val tmptypeEq : (tmptype * tmptype) -> bool
    val tmptypeFVIn : tmptype -> int -> bool
    val layout : top_level -> string
end

functor TypedAst (Atoms : ATOMS) : TYPED_AST =
struct
structure Atoms = Atoms
datatype tmptype =
         TmpVar of int
         | TmpInt
         | TmpReal
         | TmpUnit
         | TmpProduct of tmptype * tmptype
         | TmpArrow of tmptype * tmptype
         | TmpList of tmptype

datatype exp =
         Var of tmptype * Atoms.Id.t
         | Pair of tmptype * exp * exp
         | Fst of tmptype * exp
         | Snd of tmptype * exp
         | Ifte of tmptype * exp * exp * exp
         | App of tmptype * exp * exp
         | Abs of tmptype * Atoms.Id.t * Atoms.Type.t * exp
         | Con of tmptype * Atoms.Const.t
         | Op of tmptype * Atoms.Operator.t * exp * exp
         | Map of tmptype * exp * exp
         | Foldl of tmptype * exp * exp * exp
         | Mapi of tmptype * exp * exp
         | Foldli of tmptype * exp * exp * exp
         | Nth of tmptype * exp
         | Loop of tmptype * exp * exp * exp
         | Unit of tmptype

type top_level = exp
open Atoms
fun tmptypeLayout (TmpVar i) = "T" ^ (Int.toString i)
  | tmptypeLayout TmpInt = "int"
  | tmptypeLayout TmpReal = "real"
  | tmptypeLayout TmpUnit = "unit"
  | tmptypeLayout (TmpProduct (t1, t2)) = "(" ^ (tmptypeLayout t1) ^ " * " ^ (tmptypeLayout t2) ^ ")"
  | tmptypeLayout (TmpArrow (t1, t2)) = "(" ^ (tmptypeLayout t1) ^ " -> " ^ (tmptypeLayout t2) ^ ")"
  | tmptypeLayout (TmpList t) = "(" ^ (tmptypeLayout t) ^ " list) "

fun tmptypeEq (TmpVar i, TmpVar j) = (i = j)
  | tmptypeEq (TmpInt, TmpInt)  = true
  | tmptypeEq (TmpReal, TmpReal)  = true
  | tmptypeEq (TmpUnit, TmpUnit)  = true
  | tmptypeEq (TmpProduct (t11, t12), TmpProduct (t21, t22)) =
    (tmptypeEq (t11, t21)) andalso (tmptypeEq (t12, t22))
  | tmptypeEq (TmpArrow (t11, t12), TmpArrow (t21, t22)) =
    (tmptypeEq (t11, t21)) andalso (tmptypeEq (t12, t22))
  | tmptypeEq (TmpList t1, TmpList t2) = tmptypeEq (t1, t2)
  | tmptypeEq _ = false

fun tmptypeFV (TmpVar i) = [i]
  | tmptypeFV (TmpProduct (t1, t2)) = (tmptypeFV t1) @ (tmptypeFV t2)
  | tmptypeFV (TmpArrow (t1, t2)) = (tmptypeFV t1) @ (tmptypeFV t2)
  | tmptypeFV (TmpList t) = tmptypeFV t
  | tmptypeFV _ = []

fun tmptypeFVIn t i = List.exists (fn e => e = i) (tmptypeFV t)

fun layout exp =
    case exp of
        Var (t, id) => "(" ^ (Id.layout id) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Pair (t, e1, e2) => "(" ^ (layout e1) ^ "," ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Fst (t, e) => "(" ^ "fst " ^ (layout e) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Snd (t, e) => "(" ^ "snd " ^ (layout e) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Ifte (t, e1, e2, e3) => "(" ^ "if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Con (t, c) => "(" ^ (Const.layout c) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | App(t, e1, e2) => "(" ^ "(" ^ (layout e1) ^ " " ^ (layout e2) ^ ")" ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Abs(t, id, tDsl, e) => "(" ^ "(fn ("^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ ") => " ^ (layout e) ^")" ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Op (t, oper, e1, e2) => "(" ^ (layout e1) ^ (Operator.layout oper) ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Map (t, e1, e2) => "(" ^ "map " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Foldl (t, e1, e2, e3) => "(" ^ "foldl " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Mapi (t, e1, e2) => "(" ^ "mapi " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Foldli (t, e1, e2, e3) => "(" ^ "foldli " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Nth (t, e) => "(" ^ "nth " ^ (layout e)  ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Loop (t, e1, e2, e3) => "(" ^ "loop " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Unit t => "(" ^ "() : " ^ (tmptypeLayout t) ^ ")"

(* fun layout ast = *)
(*       case ast of *)
(*           Var v => Id.layout v *)
(*         | Pair (e1, e2) => *)
(*           let val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*           in *)
(*               "("^s1^","^s2^")" *)
(*           end *)
(*         | Fst e => "(fst " ^ (layout e) ^ ")" *)
(*         | Snd e => "(snd " ^ (layout e) ^ ")" *)
(*         | Ifte (e1, e2, e3) => *)
(*           "(if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ ")" *)
(*         | Con c => Const.layout c *)
(*         | App(e1,e2) => *)
(*           let val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*           in *)
(*               "("^s1^" "^s2^")" *)
(*           end *)
(*         | Abs(id, t, e) => *)
(*           let val s = layout e *)
(*           in *)
(*               "(fn ("^ (Id.layout id) ^ " : " ^ (Type.layout t) ^ ") => " ^ s ^")" *)
(*           end *)
(*         | Op (oper,e1, e2) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*               val s0 = Operator.layout oper *)
(*           in *)
(*               "(" ^ s1 ^ s0 ^ s2 ^ ")" *)
(*           end *)
(*         | Map (e1, e2) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*           in *)
(*               "(map " ^ s1 ^ " " ^ s2 ^ ")" *)
(*           end *)
(*         | Foldl (e1, e2, e3) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*               val s3 = layout e3 *)
(*           in *)
(*               "(foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")" *)
(*           end *)
(*         | Mapi (e1, e2) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*           in *)
(*               "(mapi " ^ s1 ^ " " ^ s2 ^ ")" *)
(*           end *)
(*         | Foldli (e1, e2, e3) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*               val s3 = layout e3 *)
(*           in *)
(*               "(foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")" *)
(*           end *)
(*         | Nth (e1) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*           in *)
(*               "(nth " ^ s1 ^ ")" *)
(*           end *)
(*         | Loop (e1, e2, e3) => *)
(*           let *)
(*               val s1 = layout e1 *)
(*               val s2 = layout e2 *)
(*               val s3 = layout e3 *)
(*           in *)
(*               "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")" *)
(*           end *)
(*         | Unit => "()" *)
end

structure TypedAst = TypedAst(Atoms);
