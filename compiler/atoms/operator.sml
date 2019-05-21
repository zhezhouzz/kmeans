signature OPERATOR =
sig
    (* type t *)
    datatype t =
             Add
             | Mul
             | Divi
             | Less
             | Eq
             | Greater
    val ar : t -> int
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
fun layout Add = "+"
  | layout Mul = "*"
  | layout Divi = "/"
  | layout Less = "<"
  | layout Eq = "="
  | layout Greater = ">"
end
