signature APPR_METHOD =
sig
    structure TmpType : TMP_TYPE
    datatype t =
             Map of TmpType.t * TmpType.t
             | Foldl of TmpType.t
             | Mapi of TmpType.t * TmpType.t
             | Foldli of TmpType.t
             | Nth of TmpType.t
             | Sample of TmpType.t
    val layout : t -> string
    val typeLayout : TmpType.t -> string
    val requiredType : t -> TmpType.t
end

structure ApprMethod : APPR_METHOD =
struct
structure TmpType = TmpType
datatype t =
         Map of TmpType.t * TmpType.t
         | Foldl of TmpType.t
         | Mapi of TmpType.t * TmpType.t
         | Foldli of TmpType.t
         | Nth of TmpType.t
         | Sample of TmpType.t
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
      | Sample t => (typeLayout t) ^ ".sample"
fun requiredType t =
    case t of
        Map (t1, t2) => TmpType.TmpArrow (t1, t2)
      | Foldl t => t
      | Mapi (t1, t2) => TmpType.TmpArrow (t1, t2)
      | Foldli t => t
      | Nth t => t
      | Sample t => t
end
