(printIntLn: (int -> unit)) (foldl (fn x : int * int => ((fst x) + (snd x))) 0 (data : int list))
