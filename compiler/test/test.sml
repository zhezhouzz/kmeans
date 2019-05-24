structure B = ApprBool;
structure I = ApprInt;
structure R = ApprReal;
structure pIlp =
ApprList(structure A = I);
let
    val data = [1, 2, 3, 4]
    val data = pIlp.sample fullSample data
    val _ = (print (Int.toString (pIlp.foldl (fn x => ((fst x)+(snd x))) 0 data)))
in
    ()
end;
