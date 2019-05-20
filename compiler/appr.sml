signature APPR_PASS =
sig
    structure DslAst : DSL_AST
    structure ApprSmlAst : APPR_SML_AST
    sharing DslAst.Atoms = ApprSmlAst.Atoms
    val pass : DslAst.top_level -> ApprSmlAst.top_level
end

structure ApprPass : APPR_PASS =
struct
structure DslAst = AstDsl
structure ApprSmlAst = AstApprSml
fun pass ast =
    case ast of
        DslAst.AbsAppr(id, t, distr, e) =>
        ApprSmlAst.Abs (id, t, ApprSmlAst.App (
                            ApprSmlAst.Abs (id, t, pass e),
                            ApprSmlAst.App (ApprSmlAst.Var distr, ApprSmlAst.Var id)
                        ))
      | DslAst.Var v => ApprSmlAst.Var v
      | DslAst.Pair (e1, e2) => ApprSmlAst.Pair (pass e1, pass e2)
      | DslAst.Fst e => ApprSmlAst.Fst (pass e)
      | DslAst.Snd e => ApprSmlAst.Snd (pass e)
      | DslAst.Ifte (e1, e2, e3) => ApprSmlAst.Ifte (pass e1, pass e2, pass e3)
      | DslAst.Con c => ApprSmlAst.Con c
      | DslAst.App (e1, e2) => ApprSmlAst.App (pass e1, pass e2)
      | DslAst.Abs (v, t, e) => ApprSmlAst.Abs (v, t, pass e)
      | DslAst.Op (oper, e1, e2) => ApprSmlAst.Op (oper, pass e1, pass e2)
      | DslAst.Map (e1, e2) => ApprSmlAst.Map (pass e1, pass e2)
      | DslAst.Foldl (e1, e2, e3) => ApprSmlAst.Foldl (pass e1, pass e2, pass e3)
      | DslAst.Mapi (e1, e2) => ApprSmlAst.Mapi (pass e1, pass e2)
      | DslAst.Foldli (e1, e2, e3) => ApprSmlAst.Foldli (pass e1, pass e2, pass e3)
      | DslAst.Nth (e) => ApprSmlAst.Nth (pass e)
      | DslAst.Loop (e1, e2, e3) => ApprSmlAst.Loop (pass e1, pass e2, pass e3)
      | DslAst.Unit => ApprSmlAst.Unit
end
