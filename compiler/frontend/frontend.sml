structure DslAst = DslAst(structure Atoms = Atoms
                          structure Type = Type);
structure TypeCheck = TypeCheck(DslAst);
structure Simplify = Simplify(TypeCheck);
structure FallBack = FallBack(Simplify);
structure Approx = Approx(FallBack);
