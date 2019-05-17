fun fromFile fileName =
    let
        val inStream = TextIO.openIn fileName
        val len =
            case TextIO.inputLine inStream of
                NONE => raise Fail "Bad input format"
              | SOME s =>
                case Int.fromString s of
                    NONE => raise Fail "Bad input format"
                  | SOME x => x
        fun aux i =
            if i = len then [] else
            case TextIO.inputLine inStream of
                NONE => raise Fail "Bad input format"
              | SOME s =>
                let
                    val point =
                        case List.map Real.fromString (String.tokens (fn c => c = #" ") s) of
                            [SOME x, SOME y] => (x, y)
                          | _ => raise Fail "Bad input format"
                in
                    point :: (aux (i + 1))
                end
        val datas = aux 0
    in
        datas
    end

fun printDatas datas =
    case datas of
        [] => ()
      | (x, y) :: t =>
        let
            val _ = print ((Real.toString x) ^ " " ^ (Real.toString y) ^ "\n")
        in
            printDatas t
        end

fun centroid (rangeX, rangeY) =
    let
        val x = Mlrandom.uniformReal rangeX
        val y = Mlrandom.uniformReal rangeY
    in
        (x, y)
    end

fun centroidToString (x, y) =
    "(" ^ (Real.toString x) ^ ", " ^ (Real.toString y) ^ ")"

fun centroidsToString l =
    List.foldl (fn (e, r) =>
                   r ^ (centroidToString e) ^ "\n"
               ) "" l

fun distance (x1, y1) (x2, y2) : real =
    (x1 - x2) * (x1 - x2) +
    (y1 - y2) * (y1 - y2)

fun loop f input n =
    if n = 0 then input else
    loop f (f input) (n - 1)

fun centroidGen n (rangeX, rangeY) =
    let
        fun aux i =
            if i = n then [] else
            (centroid (rangeX, rangeY)) :: (aux (i + 1))
    in
        aux 0
    end
