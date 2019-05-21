signature ID =
sig
    type t
    val parse: string -> t
    val layout : t -> string
end

structure Id : ID =
struct
type t = string
fun parse x = x
fun layout x = x
end
