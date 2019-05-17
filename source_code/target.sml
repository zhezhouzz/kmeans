(* APPR HERE *)
structure ApprRealPairList =
ApprList(structure A =
         PairAppr(structure A = RealAppr
                  structure B = RealAppr))

structure ApprIntList =
ApprList(structure A = IntAppr)

structure ApprListMapRealPairToInt =
ApprListMap(structure LA = ApprRealPairList
            structure LB = ApprIntList);

let
    val data = fromFile "_data/psudo100.txt"
    val centroids = centroidGen 2 ((0.00, 100.00), (0.00, 100.00))
    val apprData = ApprRealPairList.sample (fn l => diffSample l 10.0) data
    val centroids =
        loop (fn centroids =>
                 let
                     val c =
                         ApprListMapRealPairToInt.map
                             (fn p =>
                                 fst (
                                     ExtendedList.foldli
                                         (fn (i, e, (i', e')) => if e < e' then (i, e) else (i', e'))
                                         (0, 999999.0)
                                         (ExtendedList.map (fn e => distance e p) centroids)
                                 )
                             ) apprData
                     val centroids =
                         ExtendedList.mapi
                             (fn (i, _) =>
                                 let
                                     val (sumX, sumY, num) =
                                         ApprRealPairList.foldli (fn (j, (x, y), (sumX, sumY, num)) =>
                                                                     (if (ApprIntList.nth (c, j)) = i
                                                                      then (sumX + x, sumY + y, num + 1)
                                                                      else (sumX, sumY, num)))
                                                                 (0.0, 0.0, 0) apprData
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


