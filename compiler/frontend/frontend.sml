signature FRONTEND =
sig
    structure DslAst : DSL_AST
    val noApprToSML : DslAst.top_level -> string
    val apprToSML : DslAst.top_level -> string
end

functor Frontend (DslAst : DSL_AST) : FRONTEND =
struct
structure DslAst = DslAst
structure Approx = Approx (FallBack (Simplify (TypeCheck (DslAst))))
open Approx
fun noApprToSML ast =
    TypedAst.layoutSML (fallback (simplify (typeCheck ast)))
fun apprToSML ast =
    ApprAst.layoutSML (approx (simplify (typeCheck ast)))
end

structure DslAst = DslAst(structure Atoms = Atoms
                          structure Type = Type);
structure Frontend = Frontend(DslAst);
