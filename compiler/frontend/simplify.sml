signature SIMPLIFY =
sig
    structure TypeCheck : TYPE_CHECK
    include TYPE_CHECK
    structure TypedAst : TYPED_AST
    sharing DslAst.Atoms = TypedAst.Atoms
    sharing DslAst.Type = TypedAst.Type
    val simplify : DslAst.top_level -> TypedAst.top_level
end

functor Simplify (TypeCheck : TYPE_CHECK) : SIMPLIFY =
struct
structure TypeCheck = TypeCheck
open TypeCheck
structure TypedAst = TypedAst(structure Atoms = DslAst.Atoms
                              structure Type = DslAst.Type)

fun pass ast =
    case ast of
        DslAst.Var (SOME t, v) => TypedAst.Var (t, v)
      | DslAst.VarD (SOME t, id, e1) => TypedAst.VarD (t, id, pass e1)
      | DslAst.Pair (SOME t, e1, e2) => TypedAst.Pair (t, pass e1, pass e2)
      | DslAst.Fst (SOME t, e1) => TypedAst.Fst (t, pass e1)
      | DslAst.Snd (SOME t, e1) => TypedAst.Snd (t, pass e1)
      | DslAst.Ifte (SOME t, e1, e2, e3) => TypedAst.Ifte (t, pass e1, pass e2, pass e3)
      | DslAst.Con (t, c) => TypedAst.Con (t, c)
      | DslAst.App (SOME t, e1, e2) => TypedAst.App (t, pass e1, pass e2)
      | DslAst.Abs (SOME t, v, ty, e) => TypedAst.Abs (t, v, ty, pass e)
      | DslAst.Op (SOME t, oper, e1, e2) => TypedAst.Op (t, oper, pass e1, pass e2)
      | DslAst.Map (SOME t, e1, e2) => TypedAst.Map (t, pass e1, pass e2)
      | DslAst.Foldl (SOME t, e1, e2, e3) => TypedAst.Foldl (t, pass e1, pass e2, pass e3)
      | DslAst.Mapi (SOME t, e1, e2) => TypedAst.Mapi (t, pass e1, pass e2)
      | DslAst.Foldli (SOME t, e1, e2, e3) => TypedAst.Foldli (t, pass e1, pass e2, pass e3)
      | DslAst.Nth (SOME t, e1, e2) => TypedAst.Nth (t, pass e1, pass e2)
      | DslAst.AMap (SOME t, e1, e2) => TypedAst.AMap (t, pass e1, pass e2)
      | DslAst.AFoldl (SOME t, e1, e2, e3) => TypedAst.AFoldl (t, pass e1, pass e2, pass e3)
      | DslAst.AMapi (SOME t, e1, e2) => TypedAst.AMapi (t, pass e1, pass e2)
      | DslAst.AFoldli (SOME t, e1, e2, e3) => TypedAst.AFoldli (t, pass e1, pass e2, pass e3)
      | DslAst.ANth (SOME t, e1, e2) => TypedAst.ANth (t, pass e1, pass e2)
      | DslAst.Loop (SOME t, e1, e2, e3) => TypedAst.Loop (t, pass e1, pass e2, pass e3)
      | _ => raise Fail "Simplify: some types are lost"

val simplify = pass
end
