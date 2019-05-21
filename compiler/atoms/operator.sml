signature OPERATOR =
sig
    type t
    val ar : t -> int
    val add : t
    val mul : t
    val divi : t
    val less : t
    val eq : t
    val greater : t
    val layout : t -> string
end

structure Operator : OPERATOR =
struct
datatype t =
         Add
       | Mul
       | Divi
       | Less
       | Eq
       | Greater
fun ar Add = 2
  | ar Mul = 2
  | ar Divi = 2
  | ar Less = 2
  | ar Eq = 2
  | ar Greater = 2
val add = Add
val mul = Mul
val divi = Divi
val less = Less
val eq = Eq
val greater = Greater
fun layout Add = "+"
  | layout Mul = "*"
  | layout Divi = "/"
  | layout Less = "<"
  | layout Eq = "="
  | layout Greater = ">"
end
