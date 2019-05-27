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
structure T = DslAst.Type
structure O = DslAst.Atoms.Operator
fun opFallback oper =
    case oper of
        O.Add => O.Add
      | O.Mul => O.Mul
      | O.Divi => O.Divi
      | O.Less => O.Less
      | O.Eq => O.Eq
      | O.Greater => O.Greater
      | O.AddR => O.Add
      | O.MulR => O.Mul
      | O.DiviR => O.Divi
      | O.LessR => O.Less
      | O.GreaterR => O.Greater
      | O.EqB => O.Eq
fun pass ast =
    case ast of
        DslAst.Var v => SmlAst.Var v
     | DslAst.ImportedVar (v, _) => SmlAst.Var v
     | DslAst.ImportedVarD (v, _, _) => SmlAst.Var v
     | DslAst.Pair (e1, e2) => SmlAst.Pair (pass e1, pass e2)
     | DslAst.Fst e => SmlAst.Fst (pass e)
     | DslAst.Snd e => SmlAst.Snd (pass e)
     | DslAst.Ifte (e1, e2, e3) => SmlAst.Ifte (pass e1, pass e2, pass e3)
     | DslAst.Con c => SmlAst.Con c
     | DslAst.App (e1, e2) => SmlAst.App (pass e1, pass e2)
     | DslAst.Abs (v, t, e) => SmlAst.Abs (v, t, pass e)
     | DslAst.Op (oper, e1, e2) => SmlAst.Op (opFallback oper, pass e1, pass e2)
     | DslAst.Map (e1, e2) => SmlAst.Map (pass e1, pass e2)
     | DslAst.Foldl (e1, e2, e3) => SmlAst.Foldl (pass e1, pass e2, pass e3)
     | DslAst.Mapi (e1, e2) => SmlAst.Mapi (pass e1, pass e2)
     | DslAst.Foldli (e1, e2, e3) => SmlAst.Foldli (pass e1, pass e2, pass e3)
     | DslAst.Nth (e1, e2) => SmlAst.Nth (pass e1, pass e2)
     | DslAst.Loop (e1, e2, e3) => SmlAst.Loop (pass e1, pass e2, pass e3)
     | DslAst.Unit => SmlAst.Unit
     | DslAst.True => SmlAst.True
     | DslAst.False => SmlAst.False
end
