let
    val l = [1, 2, 3]
    val l' = [1, 2, 4]
    val w = MLton.hash l
    val w' = MLton.hash l'
    val _ = print ((Word32.toString w) ^ " " ^ (Word32.toString w') ^ "\n")
in
    ()
end;
