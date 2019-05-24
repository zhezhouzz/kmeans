((({print : (int -> unit)} : (int -> unit)) (foldl ((fn {x : _} => ((fst (x : T5) : T4)+(snd (x : T7) : T6) : T3)) : T2) (0 : int) ({data : (int list)} : (int list) ) : T1)) : T0)
structure B = BoolAppr;
structure I = IntAppr;
structure R = RealAppr;
structure pIlp =
ApprList(structure A = I);
val _ = (print (pIlp.foldl (fn x => ((fst x)+(snd x))) 0 data));

