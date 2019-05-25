signature TYPED_AST =
sig
    structure Atoms: ATOMS
    structure TmpType : TMP_TYPE

    datatype exp =
             Var of TmpType.t * Atoms.Id.t
             | ImportedVar of TmpType.t * Atoms.Id.t * Atoms.Type.t
             | ImportedVarD of TmpType.t * Atoms.Id.t * Atoms.Type.t * exp
             | Pair of TmpType.t * exp * exp
             | Fst of TmpType.t * exp
             | Snd of TmpType.t * exp
             | Ifte of TmpType.t * exp * exp * exp
             | App of TmpType.t * exp * exp
             | Abs of TmpType.t * Atoms.Id.t * Atoms.Type.t * exp
             | Con of TmpType.t * Atoms.Const.t
             | Op of TmpType.t * Atoms.Operator.t * exp * exp
             | Map of TmpType.t * exp * exp
             | Foldl of TmpType.t * exp * exp * exp
             | Mapi of TmpType.t * exp * exp
             | Foldli of TmpType.t * exp * exp * exp
             | Nth of TmpType.t * exp * exp
             | Loop of TmpType.t * exp * exp * exp
             | Unit of TmpType.t
             | True of TmpType.t
             | False of TmpType.t
    type top_level = exp
    val getType : exp -> TmpType.t
    val layout : top_level -> string
end

functor TypedAst (Atoms : ATOMS) : TYPED_AST =
struct
structure Atoms = Atoms
structure TmpType = TmpType

datatype exp =
         Var of TmpType.t * Atoms.Id.t
         | ImportedVar of TmpType.t * Atoms.Id.t * Atoms.Type.t
         | ImportedVarD of TmpType.t * Atoms.Id.t * Atoms.Type.t * exp
         | Pair of TmpType.t * exp * exp
         | Fst of TmpType.t * exp
         | Snd of TmpType.t * exp
         | Ifte of TmpType.t * exp * exp * exp
         | App of TmpType.t * exp * exp
         | Abs of TmpType.t * Atoms.Id.t * Atoms.Type.t * exp
         | Con of TmpType.t * Atoms.Const.t
         | Op of TmpType.t * Atoms.Operator.t * exp * exp
         | Map of TmpType.t * exp * exp
         | Foldl of TmpType.t * exp * exp * exp
         | Mapi of TmpType.t * exp * exp
         | Foldli of TmpType.t * exp * exp * exp
         | Nth of TmpType.t * exp * exp
         | Loop of TmpType.t * exp * exp * exp
         | Unit of TmpType.t
         | True of TmpType.t
         | False of TmpType.t
type top_level = exp
open Atoms

fun getType exp =
    case exp of
        Var (t, id) => t
      | ImportedVar (t, id, tDsl) => t
      | ImportedVarD (t, id, tDsl, e) => t
      | Pair (t, e1, e2) => t
      | Fst (t, e) => t
      | Snd (t, e) => t
      | Ifte (t, e1, e2, e3) => t
      | Con (t, c) => t
      | App(t, e1, e2) => t
      | Abs(t, id, tDsl, e) => t
      | Op (t, oper, e1, e2) => t
      | Map (t, e1, e2) => t
      | Foldl (t, e1, e2, e3) => t
      | Mapi (t, e1, e2) => t
      | Foldli (t, e1, e2, e3) => t
      | Nth (t, e1, e2) => t
      | Loop (t, e1, e2, e3) => t
      | Unit t => t
      | True t => t
      | False t => t

fun layout exp =
    case exp of
        Var (t, id) => "(" ^ (Id.layout id) ^ " : " ^ (TmpType.layout t) ^ ")"
      | ImportedVar (t, id, tDsl) => "({" ^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ "}" ^ " : " ^ (TmpType.layout t) ^ ")"
      | ImportedVarD (t, id, tDsl, e) => "({" ^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ "}" ^ " : " ^ (TmpType.layout t) ^ " : " ^ (layout e) ^")"
      | Pair (t, e1, e2) => "(" ^ (layout e1) ^ "," ^ (layout e2) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Fst (t, e) => "(" ^ "fst " ^ (layout e) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Snd (t, e) => "(" ^ "snd " ^ (layout e) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Ifte (t, e1, e2, e3) => "(" ^ "if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Con (t, c) => "(" ^ (Const.layout c) ^ " : " ^ (TmpType.layout t) ^ ")"
      | App(t, e1, e2) => "(" ^ "(" ^ (layout e1) ^ " " ^ (layout e2) ^ ")" ^ " : " ^ (TmpType.layout t) ^ ")"
      | Abs(t, id, tDsl, e) => "(" ^ "(fn {"^ (Id.layout id) ^ " : " ^ (Type.layout tDsl) ^ "} => " ^ (layout e) ^")" ^ " : " ^ (TmpType.layout t) ^ ")"
      | Op (t, oper, e1, e2) => "(" ^ (layout e1) ^ (Operator.layout oper) ^ (layout e2) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Map (t, e1, e2) => "(" ^ "map " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Foldl (t, e1, e2, e3) => "(" ^ "foldl " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Mapi (t, e1, e2) => "(" ^ "mapi " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Foldli (t, e1, e2, e3) => "(" ^ "foldli " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Nth (t, e1, e2) => "(" ^ "nth " ^ (layout e1) ^ " " ^ (layout e2) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Loop (t, e1, e2, e3) => "(" ^ "loop " ^ (layout e1) ^ " " ^ (layout e2) ^ " " ^ (layout e3) ^ " : " ^ (TmpType.layout t) ^ ")"
      | Unit t => "(" ^"() : " ^ (TmpType.layout t) ^ ")"
      | True t => "(" ^ "true : " ^ (TmpType.layout t) ^ ")"
      | False t => "(" ^ "false : " ^ (TmpType.layout t) ^ ")"
end

structure TypedAst = TypedAst(Atoms);
