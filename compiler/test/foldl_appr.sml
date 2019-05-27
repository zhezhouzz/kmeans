(printIntLn: (int -> unit))
    (
      afoldl
          (fn x : int * int => ((fst x) + (snd x)))
          0
          (data : int apprlist : (fn x : (int list) => 1.0))
    )
