signature APPR_VARIABLE =
sig
    structure TmpType : TMP_TYPE
    structure Atoms : ATOMS
    datatype t = TmpType.t * Atoms.Id.t
    val layout : t -> string
end

structure ApprVariable : APPR_VARIABLE =
struct
structure TmpType = TmpType
structure Atoms : ATOMS
datatype t = TmpType.t * Atoms.Id.t
fun layout t =
    
end
