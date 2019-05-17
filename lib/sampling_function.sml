fun fullSample l =
    ExtendedList.mapi (fn (i, e) => (i, e)) l

fun halfSample l =
    List.filter (fn (i, _) => (i mod 4) > 0) (fullSample l)

fun diffSample l d =
    let
        val (_, apprl) =
            ExtendedList.foldli (fn (i, e, (e', r)) =>
                                    if (distance e e') < d then (e, r) else
                                    (e, r @ [(i, e)]))
                                 ((9999.0, 9999.0), []) l
        val _ = print ("APPR number: " ^ (Int.toString (List.length apprl)) ^ "\n")
    in
        apprl
    end
