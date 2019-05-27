signature FORMAT =
sig
    val paren : string -> string
    val colon : string * string -> string
    val comma : string * string -> string
    val spaces : string list -> string
end

structure Format =
struct
fun paren s = "(" ^ s ^ ")"
fun colon (s1, s2) = s1 ^ " : " ^ s2
fun comma (s1, s2) = s1 ^ ", " ^ s2
fun spaces l =
    let
        val s =
            List.foldl (fn (e, r) =>
                           case r of
                               NONE => SOME e
                             | SOME r => SOME (r ^ " " ^ e)
                       ) NONE l
    in
        case s of
            NONE => ""
         | SOME s => s
    end
end
