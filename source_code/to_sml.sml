val _ =
(fn data =>
    (
      (fn centroids =>
          (
            (fn centroidsnew => (print (centroidsToString centroidsnew)))
                (loop
                     (fn centroids =>
                       ((fn c =>
                           (ExtendedList.mapi
                                (fn k =>
                                    (
                                      (fn t => ((fn numr => (((fst (fst t))/numr),((snd (fst t))/numr))) (Real.fromInt (snd t))))
                                          (ExtendedList.foldli
                                               (fn e =>
                                                   (
                                                     if ((ExtendedList.nth (c,(fst (fst e))))=(fst k))
                                                     then
                                                         (
                                                           (((fst (fst (snd e)))+(fst (snd (fst e)))),((snd (fst (snd e)))+(snd (snd (fst e))))
                                                           ),
                                                           ((snd (snd e))+1)
                                                         )
                                                     else (((fst (fst (snd e))),(snd (fst (snd e)))),(snd (snd e)))
                                                   )
                                               )
                                               ((0.0,0.0),0)
                                               data
                                          )
                                    )
                                )
                                centroids
                           )
                       )
                           (ExtendedList.map
                                (fn p =>
                                    (fst
                                         (ExtendedList.foldli
                                              (fn e => (if ((snd (fst e))<(snd (snd e))) then (fst e) else (snd e)))
                                              (0,999999.0)
                                              (ExtendedList.map (fn e => (distance (e,p))) centroids)
                                         )
                                    )
                                )
                                data
                           )
                     ))
                     centroids
                     3
                )
          )
      )
          (centroidGenNum 2)
    )
)
    (fromFileNum 100);
