signature DSL_AST =
sig
    structure Atoms: ATOMS
    structure Type : TYPE

    datatype exp =
             Var of (Type.t option) * Atoms.Id.t
             | VarD of (Type.t option) * Atoms.Id.t * exp
             | Pair of (Type.t option) * exp * exp
             | Fst of (Type.t option) * exp
             | Snd of (Type.t option) * exp
             | Ifte of (Type.t option) * exp * exp * exp
             | App of (Type.t option) * exp * exp
             | Abs of (Type.t option) * Atoms.Id.t * Type.t * exp
             | Con of Type.t * Atoms.Const.t
             | Op of (Type.t option) * Atoms.Operator.t * exp * exp
             | Map of (Type.t option) * exp * exp
             | Foldl of (Type.t option) * exp * exp * exp
             | Mapi of (Type.t option) * exp * exp
             | Foldli of (Type.t option) * exp * exp * exp
             | Nth of (Type.t option) * exp * exp
             | AMap of (Type.t option) * exp * exp
             | AFoldl of (Type.t option) * exp * exp * exp
             | AMapi of (Type.t option) * exp * exp
             | AFoldli of (Type.t option) * exp * exp * exp
             | ANth of (Type.t option) * exp * exp
             | Loop of (Type.t option) * exp * exp * exp
    type top_level = exp
    val getType : exp -> Type.t option
    val layout : top_level -> string
end

functor DslAst (structure Atoms : ATOMS
                structure Type : TYPE) : DSL_AST =
struct
structure Atoms = Atoms
structure Type = Type
datatype exp =
         Var of (Type.t option) * Atoms.Id.t
         | VarD of (Type.t option) * Atoms.Id.t * exp
         | Pair of (Type.t option) * exp * exp
         | Fst of (Type.t option) * exp
         | Snd of (Type.t option) * exp
         | Ifte of (Type.t option) * exp * exp * exp
         | App of (Type.t option) * exp * exp
         | Abs of (Type.t option) * Atoms.Id.t * Type.t * exp
         | Con of Type.t * Atoms.Const.t
         | Op of (Type.t option) * Atoms.Operator.t * exp * exp
         | Map of (Type.t option) * exp * exp
         | Foldl of (Type.t option) * exp * exp * exp
         | Mapi of (Type.t option) * exp * exp
         | Foldli of (Type.t option) * exp * exp * exp
         | Nth of (Type.t option) * exp * exp
         | AMap of (Type.t option) * exp * exp
         | AFoldl of (Type.t option) * exp * exp * exp
         | AMapi of (Type.t option) * exp * exp
         | AFoldli of (Type.t option) * exp * exp * exp
         | ANth of (Type.t option) * exp * exp
         | Loop of (Type.t option) * exp * exp * exp
type top_level = exp

open Atoms

fun getType ast =
    case ast of
        Var (t, id) => t
      | VarD (t, id, e) => t
      | Pair (t, e1, e2) => t
      | Fst (t, e1) => t
      | Snd (t, e1) => t
      | Ifte (t, e1, e2, e3) => t
      | Con (t, c) => SOME t
      | App (t, e1, e2) => t
      | Abs(t, id, ty, e) => t
      | Op (t, oper,e1, e2) => t
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

open Format
fun layout ast =
    case ast of
        Var (t, id) =>
        (case t of
             NONE => Id.layout id
           | SOME t => paren (colon (Id.layout id, Type.layout t))
        )
      | VarD (t, id, _) =>
        (case t of
             NONE => Id.layout id
           | SOME t => paren (colon (Id.layout id, Type.layout t))
        )
      | Pair (_, e1, e2) => paren (comma (layout e1, layout e2))
      | Fst (_, e1) => paren (spaces ["fst", layout e1])
      | Snd (_, e1) => paren (spaces ["snd", layout e1])
      | Ifte (_, e1, e2, e3) =>
        paren (spaces ["if", layout e1, "then", layout e2, "else", layout e3])
      | Con (t, c) => paren (colon (Const.layout c, Type.layout t))
      | App (_, e1, e2) => paren (spaces [layout e1, layout e2])
      | Abs(_, id, t, e1) => paren (spaces ["fn", (colon (Id.layout id, Type.layout t)), "=>", layout e1])
      | Op (_, oper,e1, e2) => paren (spaces [layout e1, Operator.layout oper, layout e2])
      | Map (_, e1, e2) => paren (spaces ["map", layout e1, layout e2])
      | Foldl (_, e1, e2, e3) => paren (spaces ["foldl", layout e1, layout e2, layout e3])
      | Mapi (_, e1, e2) => paren (spaces ["mapi", layout e1, layout e2])
      | Foldli (_, e1, e2, e3) => paren (spaces ["foldli", layout e1, layout e2, layout e3])
      | Nth (_, e1, e2) => paren (spaces ["nth", paren (comma (layout e1, layout e2))])
      | AMap (_, e1, e2) => paren (spaces ["amap", layout e1, layout e2])
      | AFoldl (_, e1, e2, e3) => paren (spaces ["afoldl", layout e1, layout e2, layout e3])
      | AMapi (_, e1, e2) => paren (spaces ["amapi", layout e1, layout e2])
      | AFoldli (_, e1, e2, e3) => paren (spaces ["afoldli", layout e1, layout e2, layout e3])
      | ANth (_, e1, e2) => paren (spaces ["anth", paren (comma (layout e1, layout e2))])
      | Loop (_, e1, e2, e3) => paren (spaces ["loop", layout e1, layout e2, layout e3])
end
