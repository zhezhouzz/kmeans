signature FALL_BACK_PASS =
sig
    structure DslAst : DSL_AST
    structure SmlAst : SML_AST
    sharing DslAst.Atoms = SmlAst.Atoms
    val pass : DslAst.top_level -> SmlAst.top_level
end

structure FallBackPass : FALL_BACK_PASS =
struct
structure DslAst = DslAst
structure SmlAst = SmlAst
fun pass ast =
    case ast of
        DslAst.Var v => SmlAst.Var v
      | DslAst.Pair (e1, e2) => SmlAst.Pair (pass e1, pass e2)
      | DslAst.Fst e => SmlAst.Fst (pass e)
      | DslAst.Snd e => SmlAst.Snd (pass e)
      | DslAst.Ifte (e1, e2, e3) => SmlAst.Ifte (pass e1, pass e2, pass e3)
      | DslAst.Con c => SmlAst.Con c
      | DslAst.App (e1, e2) => SmlAst.App (pass e1, pass e2)
      | DslAst.Abs (v, t, e) => SmlAst.Abs (v, t, pass e)
      | DslAst.Op (oper, e1, e2) => SmlAst.Op (oper, pass e1, pass e2)
      | DslAst.Map (e1, e2) => SmlAst.Map (pass e1, pass e2)
      | DslAst.Foldl (e1, e2, e3) => SmlAst.Foldl (pass e1, pass e2, pass e3)
      | DslAst.Mapi (e1, e2) => SmlAst.Mapi (pass e1, pass e2)
      | DslAst.Foldli (e1, e2, e3) => SmlAst.Foldli (pass e1, pass e2, pass e3)
      | DslAst.Nth (e) => SmlAst.Nth (pass e)
      | DslAst.Loop (e1, e2, e3) => SmlAst.Loop (pass e1, pass e2, pass e3)
      | DslAst.Unit => SmlAst.Unit
end
