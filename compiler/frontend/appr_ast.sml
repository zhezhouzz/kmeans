signature APPR_AST =
sig
    structure Atoms: ATOMS
    structure ApprKernel : APPR_KERNEL

    datatype exp =
             Var of ApprKernel.Type.t * Atoms.Id.t
             | Pair of ApprKernel.Type.t * exp * exp
             | Fst of ApprKernel.Type.t * exp
             | Snd of ApprKernel.Type.t * exp
             | Ifte of ApprKernel.Type.t * exp * exp * exp
             | App of ApprKernel.Type.t * exp * exp
             | Abs of ApprKernel.Type.t * Atoms.Id.t * ApprKernel.Type.t * exp
             | Con of ApprKernel.Type.t * Atoms.Const.t
             | Op of ApprKernel.Type.t * Atoms.Operator.t * exp * exp
             | Map of ApprKernel.Type.t * exp * exp
             | Foldl of ApprKernel.Type.t * exp * exp * exp
             | Mapi of ApprKernel.Type.t * exp * exp
             | Foldli of ApprKernel.Type.t * exp * exp * exp
             | Nth of ApprKernel.Type.t * exp * exp
             | ASample of ApprKernel.Type.t * ApprKernel.ApprType.t * ApprKernel.Type.t * Atoms.Id.t * exp
             | AMap of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
             | AFoldl of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp * exp
             | AMapi of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
             | AFoldli of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp * exp
             | ANth of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
             | Loop of ApprKernel.Type.t * exp * exp * exp
    type top_level = exp
    val layoutSML : top_level -> string
end

functor ApprAst (structure Atoms : ATOMS
                 structure Type : TYPE) : APPR_AST =
struct
structure Atoms = Atoms
structure ApprKernel = ApprKernel(ApprType(Type))
datatype exp =
         Var of ApprKernel.Type.t * Atoms.Id.t
         | Pair of ApprKernel.Type.t * exp * exp
         | Fst of ApprKernel.Type.t * exp
         | Snd of ApprKernel.Type.t * exp
         | Ifte of ApprKernel.Type.t * exp * exp * exp
         | App of ApprKernel.Type.t * exp * exp
         | Abs of ApprKernel.Type.t * Atoms.Id.t * ApprKernel.Type.t * exp
         | Con of ApprKernel.Type.t * Atoms.Const.t
         | Op of ApprKernel.Type.t * Atoms.Operator.t * exp * exp
         | Map of ApprKernel.Type.t * exp * exp
         | Foldl of ApprKernel.Type.t * exp * exp * exp
         | Mapi of ApprKernel.Type.t * exp * exp
         | Foldli of ApprKernel.Type.t * exp * exp * exp
         | Nth of ApprKernel.Type.t * exp * exp
         | ASample of ApprKernel.Type.t * ApprKernel.ApprType.t * ApprKernel.Type.t * Atoms.Id.t * exp
         | AMap of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
         | AFoldl of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp * exp
         | AMapi of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
         | AFoldli of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp * exp
         | ANth of ApprKernel.Type.t * ApprKernel.ApprType.t * exp * exp
         | Loop of ApprKernel.Type.t * exp * exp * exp
type top_level = exp
open Atoms
open Format

fun layoutSML ast =
    let
        val header = ApprKernel.init ()
        fun layout ast =
            case ast of
                Var (t, v) => Id.layout v
              | Pair (_, e1, e2) => peran (comma (layout e1, layout e2))
              | Fst (_, e1) => peran (spaces ["fst", layout e1])
              | Snd (_, e1) => peran (spaces ["snd", layout e1])
              | Ifte (_, e1, e2, e3) =>
                peran (spaces ["if", layout e1, "then", layout e2, "else", layout e3])
              | Con (t, c) => peran (colon (Const.layout c, Type.layout t))
              | App (_, e1, e2) => peran (spaces [layout e1, layout e2])
              | Abs(_, id, t, e1) => peran (spaces ["fn", Id.layout id, "=>", layout e1])
              | Op (_, oper,e1, e2) => peran (spaces [layout e1, Operator.layout (Operator.fallback oper), layout e2])
              | Map (_, e1, e2) => peran (spaces ["ExtendedList.map", layout e1, layout e2])
              | Foldl (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldl", layout e1, layout e2, layout e3])
              | Mapi (_, e1, e2) => peran (spaces ["ExtendedList.mapi", layout e1, layout e2])
              | Foldli (_, e1, e2, e3) => peran (spaces ["ExtendedList.foldli", layout e1, layout e2, layout e3])
              | Nth (_, e1, e2) => peran (spaces ["ExtendedList.nth", peran (comma (layout e1, layout e2))])
              | ASample (_, ty, t1, id, e1) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout ty
                in
                    peran (spaces [name, layout e1, peran (colon (Id.layout id, Type.layout t1))])
                end
              | AMap (_, ty, e1, e2) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout (ty)
                in
                    peran (spaces [name, layout e1, layout e2])
                end
              | AFoldl (_, ty, e1, e2, e3) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout (ty)
                in
                    peran (spaces [name, layout e1, layout e2, layout e3])
                end
              | AMapi (_, ty, e1, e2) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout (ty)
                in
                    peran (spaces [name, layout e1, layout e2])
                end
              | AFoldli (_, ty, e1, e2, e3) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout (ty)
                in
                    peran (spaces [name, layout e1, layout e2, layout e3])
                end
              | ANth (_, ty, e1, e2) =>
                let
                    val _ = ApprKernel.register (header, ty)
                    val name = ApprKernel.layout (ty)
                in
                    peran (spaces [name, peran (comma (layout e1, layout e2))])
                end
              | Loop (_, e1, e2, e3) => peran (spaces ["loop", layout e1, layout e2, layout e3])
        val body = layout ast
        val header = ApprKernel.header header
    in
        header ^ body ^ ";"
    end
end
