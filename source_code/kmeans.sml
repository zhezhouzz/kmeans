afoldl
    (fn x : (real * real) * unit => (printRealRealLn : (real * real) -> unit) (fst x))
    ()
    (
      (fn data : (real * real) apprlist =>
          (
            loop
                (fn centroids : (real * real) apprlist =>
                    (fn c : int apprlist =>
                        (
                          amapi
                              (fn k : int * (real * real) =>
                                  (
                                    (fn t : (real * real) * int =>
                                        (
                                          (fn numr : real => (((fst (fst t)) /. numr), ((snd (fst t)) /. numr)))
                                              ((Real.fromInt : int -> real) (snd t))
                                        )
                                    )
                                        (
                                          afoldli
                                              (fn e : (int * (real * real)) * ((real * real) * int) =>
                                                  if ((anth c (fst (fst e))) = (fst k))
                                                  then
                                                      ((((fst (fst (snd e))) +. (fst (snd (fst e)))),
                                                        ((snd (fst (snd e))) +. (snd (snd (fst e))))),
                                                       ((snd (snd e)) + 1))
                                                  else
                                                      (((fst (fst (snd e))), (snd (fst (snd e)))), (snd (snd e)))
                                              )
                                              ((0.0, 0.0), 0)
                                              data
                                        )
                                  )
                              )
                              centroids
                        )
                    )
                        (amap
                             (fn p : real * real =>
                                 (fst
                                      (afoldli
                                           (fn e : (int * real) * (int * real) =>
                                               if ((snd (fst e)) <. (snd (snd e)))
                                               then
                                                   (fst e)
                                               else
                                                   (snd e)
                                           )
                                           (0, 999999.0)
                                           (amap (fn e : real * real => (distance: ((real * real) * (real * real)) -> real) (e, p)) centroids)
                                 ))
                             ) data
                        )
                )
                (centroids : (real * real) apprlist: fn x : (real * real) list => 1.0)
                3
          )
      )
          (data : (real * real) apprlist : fn x : (real * real) list => 1.0)
    )
