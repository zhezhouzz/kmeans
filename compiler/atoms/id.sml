signature ID =
sig
    type t
    val parse: string -> t
    val layout : t -> string
    val beq : t * t -> bool
end

structure Id : ID =
struct
type t = string
fun parse x = x
fun layout x = x
fun beq (t1, t2) =
    case String.compare (t1, t2) of
        EQUAL => true
      | _ => false
end
