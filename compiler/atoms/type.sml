signature TYPE =
sig
    datatype t =
             TyInt
             | TyReal
             | TyList of t
             | TyArrow of t * t
             | TyPair of t * t
             | TyUnit
             | TyBool
             | TyUnknown
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
         | TyUnit
         | TyBool
         | TyUnknown
fun layout ty =
    case ty of
    TyInt => "int"
  | TyReal => "real"
  | TyList ty1 => "(" ^ (layout ty1) ^ " list)"
  | TyArrow (ty1, ty2) => "(" ^ (layout ty1) ^ " -> " ^ (layout ty2) ^ ")"
  | TyPair (ty1, ty2) => "(" ^ (layout ty1) ^ " * " ^ (layout ty2) ^ ")"
  | TyUnit => "unit"
  | TyBool => "bool"
  | TyUnknown => "_"
end
