signature ATOMS =
sig
    structure Type : TYPE
    structure Operater : OPERATOR
    structure Const : CONST
    type id
    val stringToId : string -> id
    val idToString : id -> string
    val constInt : int -> const
    val constReal : real -> const
    (* val add : oper *)
    (* val mul : oper *)
    (* val divi : oper *)
    (* val less : oper *)
    (* val eq : oper *)
    (* val greater : oper *)
    (* val cint : ctype *)
    (* val creal : ctype *)
    (* val clist : ctype -> ctype *)
    (* val unknown : ctype *)
end

structure Atoms : ATOMS =
struct
type id = string

datatype const =
         ConstInt of int
         | ConstReal of real

fun stringToId x = x
fun idToString x = x
end
