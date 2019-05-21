signature CONST =
sig
    datatype t =
             ConstInt of int
             | ConstReal of real
    val parse : string -> t
    val layout : t -> string
end

structure Const : CONST =
struct
datatype t =
         ConstInt of int
         | ConstReal of real
fun parse str =
    if Char.contains str #"."
    then
        case Real.fromString str of
            SOME x => ConstReal x
          | NONE => raise Fail ("constFromString fail: " ^ str)
    else
        case Int.fromString str of
            SOME x => ConstInt x
          | NONE => raise Fail ("constFromString fail: " ^ str)
fun layout (ConstInt x) = Int.toString x
  | layout (ConstReal x) = Real.fmt (StringCvt.FIX (SOME 6)) x
end
