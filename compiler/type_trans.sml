signature TYPE_TRANS =
sig
    structure Type : TYPE
    structure TmpType: TMP_TYPE
    val trans : Type.t -> TmpType.t option
end

structure TypeTrans =
struct
structure Type = Type
structure TmpType = TmpType
open TmpType
fun trans t =
    case t of
        Type.TyInt => SOME TmpInt
      | Type.TyReal => SOME TmpReal
      | Type.TyBool => SOME TmpBool
      | Type.TyList t =>
        (case trans t of
             NONE => NONE
           | SOME t => SOME (TmpList t))
      | Type.TyArrow (t1, t2) =>
        (case (trans t1, trans t2) of
             (SOME t1, SOME t2) => SOME (TmpArrow (t1, t2))
           | _ => NONE
        )
      | Type.TyPair (t1, t2) =>
        (case (trans t1, trans t2) of
             (SOME t1, SOME t2) => SOME (TmpProduct (t1, t2))
           | _ => NONE
        )
      | Type.TyUnit => SOME TmpUnit
      | Type.TyUnknown => NONE
end
