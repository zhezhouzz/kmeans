signature APPR_SML_AST =
sig
    structure Atoms: ATOMS
    structure TmpType : TMP_TYPE
    structure ApprKernel : APPR_KERNEL
    sharing ApprKernel.ApprMethod.TmpType = TmpType
    sharing ApprKernel.Atoms = Atoms

    datatype exp =
             Var of Atoms.Id.t
             | ImportedVar of TmpType.t * Atoms.Id.t
             | ImportedVarD of TmpType.t * Atoms.Id.t * exp
             | Pair of exp * exp
             | Fst of exp
             | Snd of exp
             | Ifte of exp * exp * exp
             | App of exp * exp
             | Abs of Atoms.Id.t * Atoms.Type.t * exp
             | Con of Atoms.Const.t
             | Op of Atoms.Operator.t * exp * exp
             | Map of TmpType.t * exp * (TmpType.t * exp)
             | Foldl of exp * exp * (TmpType.t * exp)
             | Mapi of TmpType.t * exp * (TmpType.t * exp)
             | Foldli of exp * exp * (TmpType.t * exp)
             | Nth of (TmpType.t * exp) * exp
             | Loop of exp * exp * exp
             | Unit
             | True
             | False
    type top_level = exp
    val layout : top_level -> string
end

structure ApprSmlAst : APPR_SML_AST=
struct
structure Atoms = Atoms
structure TmpType = TmpType
structure ApprKernel = ApprKernel

datatype exp =
         Var of Atoms.Id.t
         | ImportedVar of  TmpType.t * Atoms.Id.t
         | ImportedVarD of  TmpType.t * Atoms.Id.t * exp
         | Pair of exp * exp
         | Fst of exp
         | Snd of exp
         | Ifte of exp * exp * exp
         | App of exp * exp
         | Abs of Atoms.Id.t * Atoms.Type.t * exp
         | Con of Atoms.Const.t
         | Op of Atoms.Operator.t * exp * exp
         | Map of TmpType.t * exp * (TmpType.t * exp)
         | Foldl of exp * exp * (TmpType.t * exp)
         | Mapi of TmpType.t * exp * (TmpType.t * exp)
         | Foldli of exp * exp * (TmpType.t * exp)
         | Nth of (TmpType.t * exp) * exp
         | Loop of exp * exp * exp
         | Unit
         | True
         | False
type top_level = exp

open TmpType.TmpTypeStructure

fun layout ast =
    let
        val header = ApprKernel.init ()
        fun layoutAux ast =
            case ast of
                Var x => Atoms.Id.layout x
              | ImportedVar (t, id) => Atoms.Id.layout id
              | ImportedVarD (t, id, e) =>
                (case t of
                     TmpType.TmpList _ => (ApprKernel.sample (header, ApprKernel.ApprMethod.Sample t, id); Atoms.Id.layout id)
                   | _ => raise Fail ("Only support approximation over list!\n" ^
                                      (Atoms.Id.layout id) ^ " : " ^ (TmpType.layout t) ^ " doesn't work."))
              | Pair (e1, e2) => "(" ^ (layoutAux e1) ^ "," ^ (layoutAux e2) ^")"
              | Fst e => "(fst " ^ (layoutAux e) ^ ")"
              | Snd e => "(snd " ^ (layoutAux e) ^ ")"
              | Ifte (e1, e2, e3) =>
                "(if " ^ (layoutAux e1) ^ " then " ^ (layoutAux e2) ^ " else " ^ (layoutAux e3) ^ ")"
              | Con c => Atoms.Const.layout c
              | App (e1, e2) => "("^ (layoutAux e1) ^ " " ^ (layoutAux e2) ^ ")"
              | Abs (id, _, e) => "(fn " ^ (Atoms.Id.layout id) ^ " => " ^ (layoutAux e) ^")"
              | Op (oper, e1, e2) => "(" ^ (layoutAux e1) ^ (Atoms.Operator.layout oper) ^ (layoutAux e2) ^ ")"
              | Map (t, e1, (t2, e2)) =>
                let
                    val ty = ApprKernel.ApprMethod.Map (t2, t)
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.ApprMethod.layout ty
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                in
                    "(" ^ name ^ " " ^ s1 ^ " " ^ s2 ^ ")"
                end
              | Foldl (e1, e2, (t3, e3)) =>
                let
                    val ty = ApprKernel.ApprMethod.Foldl t3
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.ApprMethod.layout ty
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                    val s3 = layoutAux e3
                in
                    "(" ^ name ^ " " ^ s1 ^ " " ^ s2 ^  " " ^ s3 ^ ")"
                end
              | Mapi (t, e1, (t2, e2)) =>
                let
                    val ty = ApprKernel.ApprMethod.Mapi (t2, t)
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.ApprMethod.layout ty
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                in
                    "(" ^ name ^ " " ^ s1 ^ " " ^ s2 ^ ")"
                end
              | Foldli (e1, e2, (t3, e3)) =>
                let
                    val ty = ApprKernel.ApprMethod.Foldli t3
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.ApprMethod.layout ty
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                    val s3 = layoutAux e3
                in
                    "(" ^ name ^ " " ^ s1 ^ " " ^ s2 ^  " " ^ s3 ^ ")"
                end
              | Nth ((t1, e1), e2) =>
                let
                    val ty = ApprKernel.ApprMethod.Nth t1
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.ApprMethod.layout ty
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                in
                    "(" ^ name ^ " (" ^ s1 ^ ", " ^ s2 ^ "))"
                end
              | Loop (e1, e2, e3) =>
                let
                    val s1 = layoutAux e1
                    val s2 = layoutAux e2
                    val s3 = layoutAux e3
                in
                    "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
                end
              | Unit => "()"
              | True => "true"
              | False => "false"
        val astStr = layoutAux ast
        val headerStr = ApprKernel.header header
    in
        headerStr ^ "val _ = " ^ astStr ^ ";\n"
    end

end
