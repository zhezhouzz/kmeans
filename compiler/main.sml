let
    val fileName =
        case CommandLine.arguments() of
            [x] => x
          | _ => raise Fail "Wrong Arguments"
    val fileStream = TextIO.openIn fileName handle Io =>
                                                   (TextIO.print ("Can't open "^fileName^".\n");
                                                    raise Fail "Exiting\n")
    val ast = Parser.parse fileStream
    (* val _ = print ((AstDsl.layout ast) ^ "\n") *)
    val _ = print ((AstDsl.astToSML ast) ^ "\n")
in
    ()
end;
