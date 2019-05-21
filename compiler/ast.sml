signature DSL_AST =
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

functor DslAst (Atoms : ATOMS) =
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
         (* | AbsAppr of id * ctype * id * exp *)
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

fun layout ast =
      case ast of
          Var v => Id.layout v
        | Pair (e1, e2) =>
          let val s1 = layout e1
              val s2 = layout e2
          in
              "("^s1^","^s2^")"
          end
        | Fst e => "(fst " ^ (layout e) ^ ")"
        | Snd e => "(snd " ^ (layout e) ^ ")"
        | Ifte (e1, e2, e3) =>
          "(if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ ")"
        | Con c => Const.layout c
        | App(e1,e2) =>
          let val s1 = layout e1
              val s2 = layout e2
          in
              "("^s1^" "^s2^")"
          end
        | Abs(id, t, e) =>
          let val s = layout e
          in
              "(fn ("^ (Id.layout id) ^ " : " ^ (Type.layout t) ^ ") => " ^ s ^")"
          end
        | Op (oper,e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s0 = Operator.layout oper
          in
              "(" ^ s1 ^ s0 ^ s2 ^ ")"
          end
        | Map (e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
          in
              "(map " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldl (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Mapi (e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
          in
              "(mapi " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldli (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Nth (e1) =>
          let
              val s1 = layout e1
          in
              "(nth " ^ s1 ^ ")"
          end
        | Loop (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Unit => "()"
end

structure DslAst = DslAst(Atoms);
