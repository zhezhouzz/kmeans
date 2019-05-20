signature ATOMS =
sig
    type id
    type const
    type oper
    type ctype
    val stringToId : string -> id
    val idToString : id -> string
    val constFromString : string -> const
    val constToString : const -> string
    val operToString : oper -> string
    val typeToString : ctype -> string
    val constInt : int -> const
    val constReal : real -> const
    val add : oper
    val mul : oper
    val divi : oper
    val less : oper
    val eq : oper
    val greater : oper
    val cint : ctype
    val creal : ctype
    val clist : ctype -> ctype
    val unknown : ctype
end

structure Atoms : ATOMS =
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
val constInt = ConstInt
val constReal = ConstReal
val add = Add
val mul = Mul
val divi = Div
val less = Less
val eq = Eq
val greater = Greater
val cint = Cint
val creal = Creal
val clist = Clist
val unknown = Unknown

fun stringToId x = x
fun idToString x = x
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
