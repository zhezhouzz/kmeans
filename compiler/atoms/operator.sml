signature OPERATOR =
sig
    datatype t =
             Add
             | Mul
             | Divi
             | Less
             | Eq
             | Greater
             | AddR
             | MulR
             | DiviR
             | LessR
             | GreaterR
             | EqB
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
         | AddR
         | MulR
         | DiviR
         | LessR
         | GreaterR
         | EqB
fun ar Add = 2
  | ar Mul = 2
  | ar Divi = 2
  | ar Less = 2
  | ar Eq = 2
  | ar Greater = 2
  | ar AddR = 2
  | ar MulR = 2
  | ar DiviR = 2
  | ar LessR = 2
  | ar GreaterR = 2
  | ar EqB = 2
fun layout Add = "+"
  | layout Mul = "*"
  | layout Divi = "/"
  | layout Less = "<"
  | layout Eq = "="
  | layout Greater = ">"
  | layout AddR = "+."
  | layout MulR = "*."
  | layout DiviR = "/."
  | layout LessR = "<."
  | layout GreaterR = ">."
  | layout EqB = "=#"
end
