signature TYPE =
sig
    type t
    val tyInt : t
    val tyReal : t
    val tyList : t -> t
    val tyArrow : t * t -> t
    val tyPair : t * t -> t
    val tyUnknown : t
    val layout : t -> string
end

structure Type : TYPE =
struct
datatype t =
         TyInt
         | TyReal
         | TyList of t
         | TyArrow of t * t
         | TyPair of t * t
         | TyUnknown
val tyInt = TyInt
val tyReal = TyReal
val tyList = TyList
val tyArrow = TyArrow
val tyPair = TyPair
val tyUnknown = TyUnknown
fun layout ty =
    case ty of
    TyInt => "int"
  | TyReal => "real"
  | TyList ty1 => "(" ^ (layout ty1) ^ " list)"
  | TyArrow (ty1, ty2) => "(" ^ (layout ty1) ^ " -> " ^ (layout ty2) ^ ")"
  | TyPair (ty1, ty2) => "(" ^ (layout ty1) ^ " * " ^ (layout ty2) ^ ")"
  | TyUnknown => "_"
end
