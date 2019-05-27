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

fun layout ast =
    case ast of
        Var (_, v) => Id.layout v
      | VarD (_, v, _) => Id.layout v
      | Pair (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "("^s1^","^s2^")"
        end
      | Fst (_, e1) => "(fst " ^ (layout e1) ^ ")"
      | Snd (_, e1) => "(snd " ^ (layout e1) ^ ")"
      | Ifte (_, e1, e2, e3) =>
        "(if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ ")"
      | Con (t, c) => "(" ^ (Const.layout c) ^ " : " ^ (Type.layout t) ^")" 
      | App (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(" ^ s1 ^ " " ^ s2 ^ ")"
        end
      | Abs(_, id, t, e) =>
        let
            val s = layout e
        in
            "(fn ("^ (Id.layout id) ^ " : " ^ (Type.layout t) ^ ") => " ^ s ^")"
        end
      | Op (_, oper,e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s0 = Operator.layout oper
        in
            "(" ^ s1 ^ s0 ^ s2 ^ ")"
        end
      | Map (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(map " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | Foldl (_, e1, e2, e3) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s3 = layout e3
        in
            "(foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
        end
      | Mapi (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(mapi " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | Foldli (_, e1, e2, e3) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s3 = layout e3
        in
            "(foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
        end
      | Nth (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(nth " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | AMap (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(map " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | AFoldl (_, e1, e2, e3) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s3 = layout e3
        in
            "(foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
        end
      | AMapi (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(mapi " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | AFoldli (_, e1, e2, e3) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s3 = layout e3
        in
            "(foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
        end
      | ANth (_, e1, e2) =>
        let
            val s1 = layout e1
            val s2 = layout e2
        in
            "(nth " ^ s1 ^ " " ^ s2 ^ ")"
        end
      | Loop (_, e1, e2, e3) =>
        let
            val s1 = layout e1
            val s2 = layout e2
            val s3 = layout e3
        in
            "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
        end
end
