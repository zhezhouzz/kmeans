(printIntLn: (int -> unit)) (foldl (fn (x: _) => ((fst x) + (snd x))) 0 (data : int list))
