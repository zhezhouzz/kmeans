let
    val fileName =
        case CommandLine.arguments() of
            [x] => x
          | _ => raise Fail "Wrong Arguments"
    val fileStream = TextIO.openIn fileName handle Io =>
                                                   (TextIO.print ("Can't open "^fileName^".\n");
                                                    raise Fail "Exiting\n")
    val ast = Parser.parse fileStream
    (* val _ = print ((DslAst.layout ast) ^ "\n") *)
    val ast = Approx.typeCheck ast
    (* val _ = print ((DslAst.layout ast) ^ "\n") *)
    val ast = Approx.simplify ast
    val _ = print ((Approx.TypedAst.layoutSML (Approx.fallback ast)) ^ "\n")
    val ast = Approx.approx ast
    val _ = print ((Approx.ApprAst.layoutSML ast) ^ "\n")
    (* val smlast = ApproxPass.pass ast *)
    (* val _ = print ((SmlAst.layout smlast) ^ "\n") *)
    (* val ast = TypeInference.inference ast *)
    (* val apprast = ApprPass.pass ast *)
    (* val _ = print ((ApprSmlAst.layout apprast) ^ "\n") *)
in
    ()
end;
