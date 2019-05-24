foldl
    (fn x => (printIntLn : int -> unit) (fst x))
    ()
(map
           (fn p =>
             (fst
                 (foldli
                      (fn e =>
                          if ((snd (fst e)) < (snd (snd e)))
                          then
                              (fst e)
                          else
                              (snd e)
                      )
                      (0, 999999.0)
                      (map (fn e => (distance: ((real * real) * (real * real)) -> real) (e, p)) (centroids : (real * real) list))
                 ))
           ) (data: (real * real) list)
      )
