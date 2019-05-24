signature APPR_TYPE =
sig
    structure TmpType : TMP_TYPE
    datatype t =
             Map of TmpType.t * TmpType.t
             | Foldl of TmpType.t
             | Mapi of TmpType.t * TmpType.t
             | Foldli of TmpType.t
             | Nth of TmpType.t
    type header
    val layout : t -> string
    val init : unit -> header
    val register : header * t -> unit
    val header : header -> string
end

structure ApprType : APPR_TYPE =
struct
structure TmpType = TmpType
datatype t =
         Map of TmpType.t * TmpType.t
         | Foldl of TmpType.t
         | Mapi of TmpType.t * TmpType.t
         | Foldli of TmpType.t
         | Nth of TmpType.t
type header = (TmpType.t list) ref
fun typeLayout t =
    case t of
        TmpType.TmpVar _ => raise Fail "unknown type"
      | TmpType.TmpInt => "I"
      | TmpType.TmpReal => "R"
      | TmpType.TmpBool => "B"
      | TmpType.TmpUnit => "U"
      | TmpType.TmpProduct (t1, t2) =>
        "p" ^ (typeLayout t1) ^ "and " ^ (typeLayout t2) ^ "p"
      | TmpType.TmpArrow (t1, t2) =>
        "p" ^ (typeLayout t1) ^ "map" ^ (typeLayout t2) ^ "p"
      | TmpType.TmpList t1 =>
        "p" ^ (typeLayout t1) ^ "l" ^ "p"
fun layout t =
    case t of
        Map (t1, t2) => (typeLayout (TmpType.TmpArrow (t1, t2))) ^ ".map"
      | Foldl t => (typeLayout t) ^ ".foldl"
      | Mapi (t1, t2) => (typeLayout (TmpType.TmpArrow (t1, t2))) ^ ".mapi"
      | Foldli t => (typeLayout t) ^ ".foldli"
      | Nth t => (typeLayout t) ^ ".nth"

fun init () = ref []
fun registerType (h, t)  =
    if List.exists (fn e => TmpType.eq (e, t)) (!h)
    then
        ()
    else
        case t of
            TmpType.TmpProduct (t1, t2) =>
            (
              registerType (h, t1);
              registerType (h, t2);
              h := (!h) @ [t]
            )
         | TmpType.TmpArrow (t1, t2) =>
            (
              registerType (h, t1);
              registerType (h, t2);
              h := (!h) @ [t]
            )
         | TmpType.TmpList t1 =>
           (
             registerType (h, t1);
             h := (!h) @ [t]
           )
         | _ => ()
fun requiredType t =
    case t of
        Map (t1, t2) => TmpType.TmpArrow (t1, t2)
      | Foldl t => t
      | Mapi (t1, t2) => TmpType.TmpArrow (t1, t2)
      | Foldli t => t
      | Nth t => t
fun register (h, t) = registerType (h, requiredType t)

fun headerAux t =
    case t of
        TmpType.TmpProduct (t1, t2) =>
        "structure " ^ (typeLayout t) ^ " =\n" ^
        "PairAppr(structure A = " ^ (typeLayout t1) ^ "\n" ^
        "structure B = " ^ (typeLayout t2) ^ ");\n"
      | TmpType.TmpArrow (t1, t2) =>
        "structure " ^ (typeLayout t) ^ " =\n" ^
        "ApprListMap(structure LA = " ^ (typeLayout t1) ^ "\n" ^
        "structure LB = " ^ (typeLayout t2) ^ ");\n"
      | TmpType.TmpList t1 =>
        "structure " ^ (typeLayout t) ^ " =\n" ^
        "ApprList(structure A = " ^ (typeLayout t1) ^ ");\n"
      | _ => ""

fun headerAtoms () =
    "structure B = ApprBool;\n" ^
    "structure I = ApprInt;\n" ^
    "structure R = ApprReal;\n"

fun header h = (headerAtoms ()) ^
               (List.foldl (fn (e, r) => r ^ (headerAux e)) "" (!h))

end
