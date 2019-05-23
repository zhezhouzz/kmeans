signature TYPED_AST =
sig
    structure Atoms: ATOMS

    datatype tmptype =
             TmpVar of int
             | TmpInt
             | TmpReal
             | TmpUnit
             | TmpBool
             | TmpProduct of tmptype * tmptype
             | TmpArrow of tmptype * tmptype
             | TmpList of tmptype

    datatype exp =
             Var of tmptype * Atoms.Id.t
             | ImportedVar of tmptype * Atoms.Id.t * Atoms.Type.t
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
             | Nth of tmptype * exp * exp
             | Loop of tmptype * exp * exp * exp
             | Unit of tmptype
             | True of tmptype
             | False of tmptype
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
         | TmpBool
         | TmpProduct of tmptype * tmptype
         | TmpArrow of tmptype * tmptype
         | TmpList of tmptype

datatype exp =
         Var of tmptype * Atoms.Id.t
         | ImportedVar of tmptype * Atoms.Id.t * Atoms.Type.t
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
         | Nth of tmptype * exp * exp
         | Loop of tmptype * exp * exp * exp
         | Unit of tmptype
         | True of tmptype
         | False of tmptype

type top_level = exp
open Atoms
fun tmptypeLayout (TmpVar i) = "T" ^ (Int.toString i)
  | tmptypeLayout TmpInt = "int"
  | tmptypeLayout TmpReal = "real"
  | tmptypeLayout TmpUnit = "unit"
  | tmptypeLayout TmpBool = "bool"
  | tmptypeLayout (TmpProduct (t1, t2)) = "(" ^ (tmptypeLayout t1) ^ " * " ^ (tmptypeLayout t2) ^ ")"
  | tmptypeLayout (TmpArrow (t1, t2)) = "(" ^ (tmptypeLayout t1) ^ " -> " ^ (tmptypeLayout t2) ^ ")"
  | tmptypeLayout (TmpList t) = "(" ^ (tmptypeLayout t) ^ " list) "

fun tmptypeEq (TmpVar i, TmpVar j) = (i = j)
  | tmptypeEq (TmpInt, TmpInt)  = true
  | tmptypeEq (TmpReal, TmpReal)  = true
  | tmptypeEq (TmpUnit, TmpUnit)  = true
  | tmptypeEq (TmpBool, TmpBool)  = true
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
      | ImportedVar (t, id, tDsl) => "({" ^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ "}" ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Pair (t, e1, e2) => "(" ^ (layout e1) ^ "," ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Fst (t, e) => "(" ^ "fst " ^ (layout e) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Snd (t, e) => "(" ^ "snd " ^ (layout e) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Ifte (t, e1, e2, e3) => "(" ^ "if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Con (t, c) => "(" ^ (Const.layout c) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | App(t, e1, e2) => "(" ^ "(" ^ (layout e1) ^ " " ^ (layout e2) ^ ")" ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Abs(t, id, tDsl, e) => "(" ^ "(fn {"^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ "} => " ^ (layout e) ^")" ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Op (t, oper, e1, e2) => "(" ^ (layout e1) ^ (Operator.layout oper) ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Map (t, e1, e2) => "(" ^ "map " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Foldl (t, e1, e2, e3) => "(" ^ "foldl " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Mapi (t, e1, e2) => "(" ^ "mapi " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Foldli (t, e1, e2, e3) => "(" ^ "foldli " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Nth (t, e1, e2) => "(" ^ "nth " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Loop (t, e1, e2, e3) => "(" ^ "loop " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (tmptypeLayout t) ^ ")"
      | Unit t => "(" ^"() : " ^ (tmptypeLayout t) ^ ")"
      | True t => "(" ^ "true : " ^ (tmptypeLayout t) ^ ")"
      | False t => "(" ^ "false : " ^ (tmptypeLayout t) ^ ")"
end

structure TypedAst = TypedAst(Atoms);
