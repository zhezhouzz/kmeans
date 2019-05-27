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
    val eq : t * t -> bool
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
fun eq (TyInt, TyInt) = true
  | eq (TyReal, TyReal) = true
  | eq (TyBool, TyBool) = true
  | eq (TyUnit, TyUnit) = true
  | eq (TyList t1, TyList t2) = eq (t1, t2)
  | eq (TyApprList t1, TyApprList t2) = eq (t1, t2)
  | eq (TyArrow (t11, t12), TyArrow (t21, t22)) = (eq (t11, t21)) andalso (eq (t12, t22))
  | eq (TyProduct (t11, t12), TyProduct (t21, t22)) = (eq (t11, t21)) andalso (eq (t12, t22))
  | eq _ = false
end
