signature APPR_KERNEL =
sig
    structure ApprType : APPR_TYPE
    include APPR_TYPE
    type header
    val method : t -> string
    val module : Type.t -> string
    val layout : t -> string
    val init : unit -> header
    val register : header * ApprType.t -> unit
    val header : header -> string
end

functor ApprKernel (ApprType : APPR_TYPE) : APPR_KERNEL =
struct
structure ApprType = ApprType
open ApprType
type header = (Type.t list) ref

fun requiredType t =
    case t of
        AMap (t1, t2) => Type.TyArrow (t1, t2)
      | AFoldl t => t
      | AMapi (t1, t2) => Type.TyArrow (t1, t2)
      | AFoldli t => t
      | ANth t => t
      | ASample t => t

fun typeLayout t =
    case t of
        Type.TyInt => "I"
      | Type.TyReal => "R"
      | Type.TyBool => "B"
      | Type.TyUnit => "U"
      | Type.TyProduct (t1, t2) =>
        "p" ^ (typeLayout t1) ^ "and" ^ (typeLayout t2) ^ "p"
      | Type.TyArrow (t1, t2) =>
        "p" ^ (typeLayout t1) ^ "map" ^ (typeLayout t2) ^ "p"
      | Type.TyList t1 =>
        "p" ^ (typeLayout t1) ^ "l" ^ "p"
      | Type.TyApprList t1 =>
        "p" ^ (typeLayout t1) ^ "al" ^ "p"

fun method (AMap _) = "map"
  | method (AFoldl _) = "foldl"
  | method (AMapi _) = "mapi"
  | method (AFoldli _) = "foldli"
  | method (ANth _) = "nth"
  | method (ASample _) = "sample"

fun module x = typeLayout x
fun layout x = (module (requiredType x)) ^ "." ^ (method x)

fun init () = ref []

fun registerType (structs, t) =
    case List.exists (fn e => Type.eq (e, t)) (!structs) of
        true => ()
      | false =>
        case t of
            Type.TyProduct (t1, t2) =>
            (
              registerType (structs, t1);
              registerType (structs, t2);
              structs := (!structs) @ [t]
            )
         | Type.TyArrow (t1, t2) =>
            (
              registerType (structs, t1);
              registerType (structs, t2);
              structs := (!structs) @ [t]
            )
         | Type.TyApprList t1 =>
           (
             registerType (structs, t1);
             structs := (!structs) @ [t]
           )
         | Type.TyList t1 =>
           (
             registerType (structs, t1);
             structs := (!structs) @ [t]
           )
         | t => structs := (!structs) @ [t]
fun register (structs, t) = registerType (structs, requiredType t)

fun headerAux t =
    case t of
        Type.TyProduct (t1, t2) =>
        "structure " ^ (module t) ^ " =\n" ^
        "ApprPair(structure A = " ^ (module t1) ^ "\n" ^
        "structure B = " ^ (module t2) ^ ");\n"
      | Type.TyArrow (t1, t2) =>
        "structure " ^ (module t) ^ " =\n" ^
        "ApprListMap(structure LA = " ^ (module t1) ^ "\n" ^
        "structure LB = " ^ (module t2) ^ ");\n"
      | Type.TyApprList t1 =>
        "structure " ^ (module t) ^ " =\n" ^
        "ApprList(structure A = " ^ (module t1) ^ ");\n"
      | Type.TyList t1 => raise Fail "No implementation for list constructor kernel."
      | Type.TyInt => "structure I = ApprInt;\n"
      | Type.TyReal => "structure R = ApprReal;\n"
      | Type.TyBool => "structure B = ApprBool;\n"
      | Type.TyUnit => "structure U = ApprUnit;\n"

fun header structs = List.foldl (fn (t, r) => r ^ (headerAux t)) "" (!structs)
end
