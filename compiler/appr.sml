signature APPR_PASS =
sig
    structure TypedAst : TYPED_AST
    structure ApprSmlAst : APPR_SML_AST
    sharing TypedAst.Atoms = ApprSmlAst.Atoms
    sharing TypedAst.TmpType = ApprSmlAst.TmpType
    val pass : TypedAst.top_level -> ApprSmlAst.top_level
end

structure ApprPass : APPR_PASS =
struct
structure TypedAst = TypedAst
structure ApprSmlAst = ApprSmlAst
fun pass ast =
    case ast of
        TypedAst.Var (_, id) => ApprSmlAst.Var id
      | TypedAst.ImportedVar (t, id, _) => ApprSmlAst.ImportedVar (t, id)
      | TypedAst.ImportedVarD (t, id, _, e) => ApprSmlAst.ImportedVarD (t, id, pass e)
      | TypedAst.Pair (_, e1, e2) => ApprSmlAst.Pair (pass e1, pass e2)
      | TypedAst.Fst (_, e) => ApprSmlAst.Fst (pass e)
      | TypedAst.Snd (_, e) => ApprSmlAst.Snd (pass e)
      | TypedAst.Ifte (_, e1, e2, e3) => ApprSmlAst.Ifte (pass e1, pass e2, pass e3)
      | TypedAst.Con (_, c) => ApprSmlAst.Con c
      | TypedAst.App (_, e1, e2) => ApprSmlAst.App (pass e1, pass e2)
      | TypedAst.Abs (_, v, t, e) => ApprSmlAst.Abs (v, t, pass e)
      | TypedAst.Op (_, oper, e1, e2) => ApprSmlAst.Op (oper, pass e1, pass e2)
      | TypedAst.Map (t, e1, e2) => ApprSmlAst.Map (t, pass e1, (TypedAst.getType e2, pass e2))
      | TypedAst.Foldl (_, e1, e2, e3) => ApprSmlAst.Foldl (pass e1, pass e2, (TypedAst.getType e3, pass e3))
      | TypedAst.Mapi (t, e1, e2) => ApprSmlAst.Mapi (t, pass e1, (TypedAst.getType e2, pass e2))
      | TypedAst.Foldli (_, e1, e2, e3) => ApprSmlAst.Foldli (pass e1, pass e2, (TypedAst.getType e3, pass e3))
      | TypedAst.Nth (_, e1, e2) => ApprSmlAst.Nth ((TypedAst.getType e1, pass e1), pass e2)
      | TypedAst.Loop (_, e1, e2, e3) => ApprSmlAst.Loop (pass e1, pass e2, pass e3)
      | TypedAst.Unit _ => ApprSmlAst.Unit
      | TypedAst.True _ => ApprSmlAst.True
      | TypedAst.False _ => ApprSmlAst.False
end
