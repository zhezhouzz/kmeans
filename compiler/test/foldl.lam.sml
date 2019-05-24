structure B = ApprBool;
structure I = ApprInt;
structure R = ApprReal;
structure pIlp =
ApprList(structure A = I);
val data = pIlp.sample fullSample data;
val _ = (printIntLn (pIlp.foldl (fn x => ((fst x)+(snd x))) 0 data));

