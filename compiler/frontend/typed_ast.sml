signature TYPED_AST =
sig
    structure Atoms: ATOMS
    structure Type : TYPE

    datatype exp =
             Var of Type.t * Atoms.Id.t
             | VarD of Type.t * Atoms.Id.t * exp
             | Pair of Type.t * exp * exp
             | Fst of Type.t * exp
             | Snd of Type.t * exp
             | Ifte of Type.t * exp * exp * exp
             | App of Type.t * exp * exp
             | Abs of Type.t * Atoms.Id.t * Type.t * exp
             | Con of Type.t * Atoms.Const.t
             | Op of Type.t * Atoms.Operator.t * exp * exp
             | Map of Type.t * exp * exp
             | Foldl of Type.t * exp * exp * exp
             | Mapi of Type.t * exp * exp
             | Foldli of Type.t * exp * exp * exp
             | Nth of Type.t * exp * exp
             | AMap of Type.t * exp * exp
             | AFoldl of Type.t * exp * exp * exp
             | AMapi of Type.t * exp * exp
             | AFoldli of Type.t * exp * exp * exp
             | ANth of Type.t * exp * exp
             | Loop of Type.t * exp * exp * exp
    type top_level = exp
    val getType : exp -> Type.t
    val layout : top_level -> string
    val layoutSML : top_level -> string
end

functor TypedAst (structure Atoms : ATOMS
                  structure Type : TYPE) : TYPED_AST =
struct
structure Atoms = Atoms
structure Type = Type
datatype exp =
         Var of Type.t * Atoms.Id.t
         | VarD of Type.t * Atoms.Id.t * exp
         | Pair of Type.t * exp * exp
         | Fst of Type.t * exp
         | Snd of Type.t * exp
         | Ifte of Type.t * exp * exp * exp
         | App of Type.t * exp * exp
         | Abs of Type.t * Atoms.Id.t * Type.t * exp
         | Con of Type.t * Atoms.Const.t
         | Op of Type.t * Atoms.Operator.t * exp * exp
         | Map of Type.t * exp * exp
         | Foldl of Type.t * exp * exp * exp
         | Mapi of Type.t * exp * exp
         | Foldli of Type.t * exp * exp * exp
         | Nth of Type.t * exp * exp
         | AMap of Type.t * exp * exp
         | AFoldl of Type.t * exp * exp * exp
         | AMapi of Type.t * exp * exp
         | AFoldli of Type.t * exp * exp * exp
         | ANth of Type.t * exp * exp
         | Loop of Type.t * exp * exp * exp
type top_level = exp

open Atoms
open Format

fun getType ast =
    case ast of
        Var (t, v) => t
      | VarD (t, v, e) => t
      | Pair (t, e1, e2) => t
      | Fst (t, e1) => t
      | Snd (t, e1) => t
      | Ifte (t, e1, e2, e3) => t
      | Con (t, c) => t
      | App (t, e1, e2) => t
      | Abs(t, id, ty, e1) => t
      | Op (t, oper, e1, e2) => t
      | Map (t, e1, e2) => t
      | Foldl (t, e1, e2, e3) => t
      | Mapi (t, e1, e2) => t
      | Foldli (t, e1, e2, e3) => t
      | Nth (t, e1, e2) => t
      | AMap (t, e1, e2) => t
      | AFoldl (t, e1, e2, e3) => t
      | AMapi (t, e1, e2) => t
      | AFoldli (t, e1, e2, e3) => t
      | ANth (t, e1, e2) => t
      | Loop (t, e1, e2, e3) => t

fun layout ast =
    case ast of
        Var (t, v) => peran (colon (Id.layout v, Type.layout t))
      | VarD (t, v, _) => peran (colon (Id.layout v, Type.layout t))
      | Pair (_, e1, e2) => peran (comma (layout e1, layout e2))
      | Fst (_, e1) => peran (spaces ["fst", layout e1])
      | Snd (_, e1) => peran (spaces ["snd", layout e1])
      | Ifte (_, e1, e2, e3) =>
        peran (spaces ["if", layout e1, "then", layout e2, "else", layout e3])
      | Con (t, c) => peran (colon (Const.layout c, Type.layout t))
      | App (_, e1, e2) => peran (spaces [layout e1, layout e2])
      | Abs(_, id, t, e1) => peran (spaces ["fn", (colon (Id.layout id, Type.layout t)), "=>", layout e1])
      | Op (_, oper,e1, e2) => peran (spaces [layout e1, Operator.layout oper, layout e2])
      | Map (_, e1, e2) => peran (spaces ["ExtendedList.map", layout e1, layout e2])
      | Foldl (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldl", layout e1, layout e2, layout e3])
      | Mapi (_, e1, e2) => peran (spaces ["ExtendedList.mapi", layout e1, layout e2])
      | Foldli (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldli", layout e1, layout e2, layout e3])
      | Nth (_, e1, e2) => peran (spaces ["ExtendedList.nth", peran (comma (layout e1, layout e2))])
      | AMap (_, e1, e2) => peran (spaces ["ExtendedList.map", layout e1, layout e2])
      | AFoldl (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldl", layout e1, layout e2, layout e3])
      | AMapi (_, e1, e2) => peran (spaces ["ExtendedList.mapi", layout e1, layout e2])
      | AFoldli (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldli", layout e1, layout e2, layout e3])
      | ANth (_, e1, e2) => peran (spaces ["ExtendedList.nth", peran (comma (layout e1, layout e2))])
      | Loop (_, e1, e2, e3) => peran (spaces ["loop", layout e1, layout e2, layout e3])

fun layoutSML ast =
    let
        fun aux ast =
            case ast of
                Var (t, v) => Var (t, v)
              | VarD (t, v, e) => Var (t, v)
              | Pair (t, e1, e2) => Pair (t, aux e1, aux e2)
              | Fst (t, e1) => Fst (t, aux e1)
              | Snd (t, e1) => Snd (t, aux e1)
              | Ifte (t, e1, e2, e3) => Ifte (t, aux e1, aux e2, aux e3)
              | Con (t, c) => Con (t, c)
              | App (t, e1, e2) => App (t, aux e1, aux e2)
              | Abs(t, id, ty, e1) => Abs(t, id, ty, aux e1)
              | Op (t, oper, e1, e2) => Op (t, Operator.fallback oper, aux e1, aux e2)
              | Map (t, e1, e2) =>  Map (t, aux e1, aux e2)
              | Foldl (t, e1, e2, e3) =>  Foldl (t, aux e1, aux e2, aux e3)
              | Mapi (t, e1, e2) => Mapi (t, aux e1, aux e2)
              | Foldli (t, e1, e2, e3) => Foldli (t, aux e1, aux e2, aux e3)
              | Nth (t, e1, e2) => Nth (t, aux e1, aux e2)
              | AMap (t, e1, e2) => Map (t, aux e1, aux e2)
              | AFoldl (t, e1, e2, e3) => Foldl (t, aux e1, aux e2, aux e3)
              | AMapi (t, e1, e2) => Mapi (t, aux e1, aux e2)
              | AFoldli (t, e1, e2, e3) => Foldli (t, aux e1, aux e2, aux e3)
              | ANth (t, e1, e2) => Nth (t, aux e1, aux e2)
              | Loop (t, e1, e2, e3) => Loop (t, aux e1, aux e2, aux e3)
        val ast = aux ast
    in
        (layout ast) ^ ";"
    end
end
