(fn data : (real * real) apprlist =>
    (fn centroids : (real * real) apprlist =>
                          (afoldli
                               (fn e : (int * real) * (int * real) =>
                                   if ((snd (fst e)) <. (snd (snd e)))
                                   then
                                       (fst e)
                                   else
                                       (snd e)
                               )
                               (0, 999999.0)
                               (amap (fn e : real * real => (distance: ((real * real) * (real * real)) -> real) (e, (p : real * real))) centroids)
            )
    )
        (centroids : (real * real) apprlist: fn x : (real * real) list => 1.0)
)
    (data : (real * real) apprlist : fn x : (real * real) list => 1.0)

