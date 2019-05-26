signature CONST =
sig
    datatype t =
             CInt of int
             | CReal of real
             | CTrue
             | CFalse
             | CUnit
    val parseNum : string -> t
    val layout : t -> string
end

structure Const : CONST =
struct
datatype t =
         CInt of int
         | CReal of real
         | CTrue
         | CFalse
         | CUnit
fun parseNum str =
    if Char.contains str #"."
    then
        case Real.fromString str of
            SOME x => CReal x
          | NONE => raise Fail ("constFromString fail: " ^ str)
    else
        case Int.fromString str of
            SOME x => CInt x
          | NONE => raise Fail ("constFromString fail: " ^ str)
fun layout (CInt x) = Int.toString x
  | layout (CReal x) = Real.fmt (StringCvt.FIX (SOME 6)) x
  | layout CTrue = "true"
  | layout CFalse = "false"
  | layout CUnit = "()"
end
