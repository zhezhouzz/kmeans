foldl
    (fn x => (printRealRealLn : (real * real) -> unit) (fst x))
    ()
    (
      (fn data =>
          (
            loop
                (fn centroids =>
                    (fn c =>
                        (
                          mapi
                              (fn k =>
                                  (
                                    (fn t =>
                                        (
                                          (fn numr => (((fst (fst t)) / numr), ((snd (fst t)) / numr)))
                                              ((Real.fromInt : int -> real) (snd t))
                                        )
                                    )
                                        (
                                          foldli
                                              (fn e =>
                                                  if ((nth c (fst (fst e))) = (fst k))
                                                  then
                                                      ((((fst (fst (snd e))) + (fst (snd (fst e)))),
                                                        ((snd (fst (snd e))) + (snd (snd (fst e))))),
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
                                           (map (fn e => (distance: ((real * real) * (real * real)) -> real) (e, p)) centroids)
                                 ))
                             ) data
                        )
                )
                (centroids : ((real * real) list))
                3
          )
      )
          (data : (real * real) list)
    )
