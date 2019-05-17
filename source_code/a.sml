let
    val (data : \appr distr) = fromFile "_data/psudo100.txt"
    val centroids = centroidGen 2 ((0.00, 100.00), (0.00, 100.00))
    val centroids =
        loop (fn centroids =>
                 let
                     val c =
                         List.map
                             (fn p =>
                                 fst (
                                     ExtendedList.foldli
                                         (fn (i, e, (i', e')) => if e < e' then (i, e) else (i', e'))
                                         (0, 999999.0)
                                         (List.map (fn e => distance e p) centroids)
                                 )
                             ) data
                     val centroids =
                         ExtendedList.mapi
                             (fn (i, _) =>
                                 let
                                     val (sumX, sumY, num) =
                                         ExtendedList.foldli (fn (j, (x, y), (sumX, sumY, num)) =>
                                                                 (if (List.nth (c, j)) = i
                                                                  then (sumX + x, sumY + y, num + 1)
                                                                  else (sumX, sumY, num)))
                                                                 (0.0, 0.0, 0) data
                                     val numr = Real.fromInt num
                                 in
                                     (sumX/numr, sumY/numr)
                                 end) centroids
                 in
                     centroids
                 end) centroids 3
    val _ = print ("Centroids:\n" ^ (centroidsToString centroids))
in
    ()
end;

