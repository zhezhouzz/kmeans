signature ATOM =
sig
    type id = string

    datatype const =
             ConstInt of int
             | ConstReal of real
    datatype oper =
             Add
             | Mul
             | Div
             | Less
             | Eq
             | Greater
    datatype ctype =
             Cint
             | Creal
             | Clist of ctype
           | Unknown
    val constFromString : string -> const
    val constToString : const -> string
    val operToString : oper -> string
    val typeToString : ctype -> string
end

structure Atom : ATOM =
struct
type id = string

datatype const =
         ConstInt of int
         | ConstReal of real
datatype oper =
         Add
         | Mul
         | Div
         | Less
         | Eq
         | Greater
datatype ctype =
         Cint
         | Creal
         | Clist of ctype
         | Unknown

fun constFromString str =
    let
        (* val _ = print ("constFromString : " ^ str ^ "\n") *)
    in
        if Char.contains str #"."
        then
            case Real.fromString str of
                SOME x => ConstReal x
              | NONE => raise Fail ("constFromString fail: " ^ str)
        else
            case Int.fromString str of
                SOME x => ConstInt x
              | NONE => raise Fail ("constFromString fail: " ^ str)
    end

fun constToString c =
    case c of
        ConstInt x => Int.toString x
      | ConstReal x => Real.fmt (StringCvt.FIX (SOME 6)) x

fun operToString oper =
    case oper of
        Add => "+"
      | Mul => "*"
      | Div => "/"
      | Less => "<"
      | Eq => "="
      | Greater => ">"

fun typeToString t =
    case t of
        Cint => "int"
      | Creal => "real"
      | Clist t =>
        "list (" ^ (typeToString t) ^ ")"
      | Unknown => "_"
end
