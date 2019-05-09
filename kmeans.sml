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
        end;

let
    val datas = fromFile "psudo.txt"
    val _ = printDatas datas
in
    ()
end;
