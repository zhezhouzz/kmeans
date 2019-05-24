signature APPR_KERNEL =
sig
    structure ApprMethod : APPR_METHOD
    structure Atoms : ATOMS
    type t
    val init : unit -> t
    val sample : t * ApprMethod.t * Atoms.Id.t -> unit
    val register : t * ApprMethod.t -> unit
    val header : t -> string
end

structure ApprKernel : APPR_KERNEL =
struct
structure ApprMethod = ApprMethod
structure Atoms = Atoms
structure HT = HashTable
type t = {structs: (TmpType.t, unit) HT.hash_table,
          variables: (Atoms.Id.t, TmpType.t) HT.hash_table}

fun init () =
    {structs = HT.mkTable ((MLton.hash, TmpType.eq), 10),
     variables = HT.mkTable ((MLton.hash, Atoms.Id.beq), 10)}

fun registerType (structs, t)  =
    case HT.find (structs, t) of
        SOME _ => ()
      | NONE =>
        case t of
            TmpType.TmpProduct (t1, t2) =>
            (
              registerType (structs, t1);
              registerType (structs, t2);
              HT.insert (structs, (t, ()))
            )
         | TmpType.TmpArrow (t1, t2) =>
            (
              registerType (structs, t1);
              registerType (structs, t2);
              HT.insert (structs, (t, ()))
            )
         | TmpType.TmpList t1 =>
           (
             registerType (structs, t1);
             HT.insert (structs, (t, ()))
           )
         | _ => ()
fun register ({structs, variables}, t) = registerType (structs, ApprMethod.requiredType t)
fun sample ({structs, variables}, t, id) =
    case t of
        ApprMethod.Sample (TmpType.TmpList t) =>
        let
            val _ = registerType (structs, TmpType.TmpList t)
        in
            case HT.find (variables, id) of
                SOME _ => raise Fail ("imported variable \"" ^ id ^ "\"has different type")
              | NONE => HT.insert (variables, (id, TmpType.TmpList t))
        end
      | _ => raise Fail "Wrong sample"

fun structLayout t =
    case t of
        TmpType.TmpProduct (t1, t2) =>
        "structure " ^ (ApprMethod.typeLayout t) ^ " =\n" ^
        "PairAppr(structure A = " ^ (ApprMethod.typeLayout t1) ^ "\n" ^
        "structure B = " ^ (ApprMethod.typeLayout t2) ^ ");\n"
      | TmpType.TmpArrow (t1, t2) =>
        "structure " ^ (ApprMethod.typeLayout t) ^ " =\n" ^
        "ApprListMap(structure LA = " ^ (ApprMethod.typeLayout t1) ^ "\n" ^
        "structure LB = " ^ (ApprMethod.typeLayout t2) ^ ");\n"
      | TmpType.TmpList t1 =>
        "structure " ^ (ApprMethod.typeLayout t) ^ " =\n" ^
        "ApprList(structure A = " ^ (ApprMethod.typeLayout t1) ^ ");\n"
      | _ => ""

fun sampleFn () = "fullSample"

fun sampleLayout (t, id) =
    "val " ^ id ^ " = " ^ (ApprMethod.layout (ApprMethod.Sample t)) ^ " " ^ (sampleFn ()) ^ " " ^ id ^ ";\n"

fun headerAtoms () =
    "structure B = ApprBool;\n" ^
    "structure I = ApprInt;\n" ^
    "structure R = ApprReal;\n"

fun header {structs, variables} =
    let
        val structsStr = (headerAtoms ()) ^ (HT.foldi (fn (t, _, r) => r ^ (structLayout t), "", structs))
        val sampleStr = HT.foldi (fn (id, t, r) => r ^ (sampleLayout (t, id)), "", variables)
    in
        structsStr ^ sampleStr
    end
end
