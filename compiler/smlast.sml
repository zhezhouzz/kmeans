signature SML_AST =
sig
    structure Atoms: ATOMS

    datatype exp =
             Var of Atoms.Id.t
             | Pair of exp * exp
             | Fst of exp
             | Snd of exp
             | Ifte of exp * exp * exp
             | App of exp * exp
             | Abs of Atoms.Id.t * Atoms.Type.t * exp
             | Con of Atoms.Const.t
             | Op of Atoms.Operator.t * exp * exp
             | Map of exp * exp
             | Foldl of exp * exp * exp
             | Mapi of exp * exp
             | Foldli of exp * exp * exp
             | Nth of exp
             | Loop of exp * exp * exp
           | Unit
    type top_level = exp
    val layout : top_level -> string
end

functor SmlAst (Atoms : ATOMS) : SML_AST=
struct
structure Atoms = Atoms
open Atoms
datatype exp =
         Var of Id.t
         | Pair of exp * exp
         | Fst of exp
         | Snd of exp
         | Ifte of exp * exp * exp
         | App of exp * exp
         | Abs of Id.t * Type.t * exp
         | Con of Const.t
         | Op of Operator.t * exp * exp
         | Map of exp * exp
         | Foldl of exp * exp * exp
         | Mapi of exp * exp
         | Foldli of exp * exp * exp
         | Nth of exp
         | Loop of exp * exp * exp
       | Unit

type top_level = exp

fun layoutAux ast =
      case ast of
          Var x => Id.layout x
        | Pair (e1, e2) => "(" ^ (layoutAux e1) ^ "," ^ (layoutAux e2) ^")"
        | Fst e => "(fst " ^ (layoutAux e) ^ ")"
        | Snd e => "(snd " ^ (layoutAux e) ^ ")"
        | Ifte (e1, e2, e3) =>
          "(if " ^ (layoutAux e1) ^ " then " ^ (layoutAux e2) ^ " else " ^ (layoutAux e3) ^ ")"
        | Con c => Const.layout c
        | App (e1, e2) => "("^ (layoutAux e1) ^ " " ^ (layoutAux e2) ^ ")"
        | Abs (id, ty, e) => "(fn " ^ (Id.layout id) ^ " : " ^ (Type.layout ty) ^ " => " ^ (layoutAux e) ^")"
        | Op (oper, e1, e2) => "(" ^ (layoutAux e1) ^ (Operator.layout oper) ^ (layoutAux e2) ^ ")"
        | Map (e1, e2) =>
          let
              val s1 = layoutAux e1
              val s2 = layoutAux e2
          in
              "(ExtendedList.map " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldl (e1, e2, e3) =>
          let
              val s1 = layoutAux e1
              val s2 = layoutAux e2
              val s3 = layoutAux e3
          in
              "(ExtendedList.foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Mapi (e1, e2) =>
          let
              val s1 = layoutAux e1
              val s2 = layoutAux e2
          in
              "(ExtendedList.mapi " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldli (e1, e2, e3) =>
          let
              val s1 = layoutAux e1
              val s2 = layoutAux e2
              val s3 = layoutAux e3
          in
              "(ExtendedList.foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Nth (e1) =>
          let
              val s1 = layoutAux e1
          in
              "(ExtendedList.nth " ^ s1  ^ ")"
          end
        | Loop (e1, e2, e3) =>
          let
              val s1 = layoutAux e1
              val s2 = layoutAux e2
              val s3 = layoutAux e3
          in
              "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Unit => "()"

fun layout ast =
    "val _ = " ^ (layoutAux ast) ^ ";\n"

end

structure SmlAst = SmlAst(Atoms);
