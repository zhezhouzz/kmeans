let
    val fileName =
        case CommandLine.arguments() of
            [x] => x
          | _ => raise Fail "Wrong Arguments"
    val fileStream = TextIO.openIn fileName handle Io =>
                                                   (TextIO.print ("Can't open "^fileName^".\n");
                                                    raise Fail "Exiting\n")
    val ast = Parser.parse fileStream
    (* val smlast = FallBackPass.pass ast *)
    (* val _ = print ((SmlAst.layout smlast) ^ "\n") *)
    val _ = TypeInference.inference ast
    (* val apprast = ApprPass.pass ast *)
    (* val _ = print ((AstApprSml.layout apprast) ^ "\n") *)
in
    ()
end;
