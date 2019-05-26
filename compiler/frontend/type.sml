signature TYPE =
sig
    datatype t =
             TyInt
             | TyReal
             | TyList of t
             | TyApprList of t
             | TyArrow of t * t
             | TyProduct of t * t
             | TyUnit
             | TyBool
    val layout : t -> string
end

structure Type : TYPE =
struct
datatype t =
         TyInt
         | TyReal
         | TyList of t
         | TyApprList of t
         | TyArrow of t * t
         | TyProduct of t * t
         | TyUnit
         | TyBool
fun layout ty =
    case ty of
    TyInt => "int"
  | TyReal => "real"
  | TyList ty1 => "(" ^ (layout ty1) ^ " list)"
  | TyApprList ty1 => "(" ^ (layout ty1) ^ " apprlist)"
  | TyArrow (ty1, ty2) => "(" ^ (layout ty1) ^ " -> " ^ (layout ty2) ^ ")"
  | TyProduct (ty1, ty2) => "(" ^ (layout ty1) ^ " * " ^ (layout ty2) ^ ")"
  | TyUnit => "unit"
  | TyBool => "bool"
end
