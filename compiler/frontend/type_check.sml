signature TYPE_CHECK =
sig
    structure DslAst : DSL_AST
    val typeCheck: DslAst.top_level -> DslAst.top_level
end

functor TypeCheck (DslAst : DSL_AST) : TYPE_CHECK =
struct
structure DslAst = DslAst
open DslAst
open Atoms

fun errorMsg loc msg =
    "Type Error in \"" ^ loc ^ "\":\n" ^ msg
fun conflictMsg t1 t2 =
    (Type.layout t1) ^ " =? " ^ (Type.layout t2)

fun fillAux ast (id0, ty0) =
    let
        fun aux ast =
            case ast of
                Var (ty, id) =>
                if Id.eq (id, id0)
                then
                    case ty of
                        NONE => Var (SOME ty0, id)
                      | SOME ty =>
                        if Type.eq (ty, ty0)
                        then ast
                        else raise Fail (errorMsg (Id.layout id) (conflictMsg ty ty0))
                else
                    ast
              (* Distribution does not share variables from others. *)
              | VarD (t, id, e) =>
                if Id.eq (id, id0)
                then raise Fail "Grammar Error: Can't use the same name as input data."
                else ast
              | Pair (t, e1, e2) => Pair (t, aux e1, aux e2)
              | Fst (t, e1) => Fst (t, aux e1)
              | Snd (t, e1) => Snd (t, aux e1)
              | Ifte (t, e1, e2, e3) => Ifte (t, aux e1, aux e2, aux e3)
              | Con (t, c) => Con (t, c)
              | App (t, e1, e2) => App (t, aux e1, aux e2)
              | Abs(t, id, ty, e) =>
                if Id.eq (id, id0)
                then ast
                else Abs(t, id, ty, aux e)
              | Op (t, oper,e1, e2) => Op (t, oper, aux e1, aux e2)
              | Map (t, e1, e2) => Map (t, aux e1, aux e2)
              | Foldl (t, e1, e2, e3) => Foldl (t, aux e1, aux e2, aux e3)
              | Mapi (t, e1, e2) => Mapi (t, aux e1, aux e2)
              | Foldli (t, e1, e2, e3) => Foldli (t, aux e1, aux e2, aux e3)
              | Nth (t, e1, e2) => Nth (t, aux e1, aux e2)
              | AMap (t, e1, e2) => AMap (t, aux e1, aux e2)
              | AFoldl (t, e1, e2, e3) => AFoldl (t, aux e1, aux e2, aux e3)
              | AMapi (t, e1, e2) => AMapi (t, aux e1, aux e2)
              | AFoldli (t, e1, e2, e3) => AFoldli (t, aux e1, aux e2, aux e3)
              | ANth (t, e1, e2) => ANth (t, aux e1, aux e2)
              | Loop (t, e1, e2, e3) => Loop (t, aux e1, aux e2, aux e3)
    in
        aux ast
    end
fun fill ast =
    case ast of
        Var (ty, id) => Var (ty, id)
      | VarD (t, id, e) => VarD (t, id, fill e)
      | Pair (t, e1, e2) => Pair (t, fill e1, fill e2)
      | Fst (t, e1) => Fst (t, fill e1)
      | Snd (t, e1) => Snd (t, fill e1)
      | Ifte (t, e1, e2, e3) => Ifte (t, fill e1, fill e2, fill e3)
      | Con (t, c) => Con (t, c)
      | App (t, e1, e2) => App (t, fill e1, fill e2)
      | Abs(t, id0, t0, e) =>
        let
            val e = fillAux e (id0, t0)
        in
            Abs(t, id0, t0, fill e)
        end
      | Op (t, oper,e1, e2) => Op (t, oper, fill e1, fill e2)
      | Map (t, e1, e2) => Map (t, fill e1, fill e2)
      | Foldl (t, e1, e2, e3) => Foldl (t, fill e1, fill e2, fill e3)
      | Mapi (t, e1, e2) => Mapi (t, fill e1, fill e2)
      | Foldli (t, e1, e2, e3) => Foldli (t, fill e1, fill e2, fill e3)
      | Nth (t, e1, e2) => Nth (t, fill e1, fill e2)
      | AMap (t, e1, e2) => AMap (t, fill e1, fill e2)
      | AFoldl (t, e1, e2, e3) => AFoldl (t, fill e1, fill e2, fill e3)
      | AMapi (t, e1, e2) => AMapi (t, fill e1, fill e2)
      | AFoldli (t, e1, e2, e3) => AFoldli (t, fill e1, fill e2, fill e3)
      | ANth (t, e1, e2) => ANth (t, fill e1, fill e2)
      | Loop (t, e1, e2, e3) => Loop (t, fill e1, fill e2, fill e3)

fun typeCheck ast =
    let
        val ast = fill ast
        (* val _ = print ((DslAst.layout ast) ^ "\n") *)
        fun aux ast =
            case ast of
                Var (t, id) =>
                (case t of
                     NONE => raise Fail (errorMsg (Id.layout id)  "Unknown type: Var")
                   | SOME t => Var (SOME t, id)
                )
              | VarD (t, id, e) =>
                let
                    val e = aux e
                in
                    case (getType e, t) of
                        (NONE, _) => raise Fail (errorMsg (Id.layout id)  "wrong distribution type")
                      | (SOME (Type.TyArrow (Type.TyList x, TyReal)), SOME t) =>
                        if Type.eq (Type.TyApprList x, t)
                        then VarD (SOME (Type.TyApprList x), id, e)
                        else raise Fail
                                   (
                                     errorMsg (Id.layout id)
                                              (conflictMsg (Type.TyApprList x) t)
                                   )
                      | (SOME (Type.TyArrow (Type.TyList x, TyReal)), NONE) =>
                        VarD (SOME (Type.TyApprList x), id, e)
                      | _ => raise Fail (errorMsg (Id.layout id)  "Unknown type: VarD")
                end
              | Pair (_, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME t1, SOME t2) => Pair (SOME (Type.TyProduct (t1, t2)), e1, e2)
                      | _ => raise Fail (errorMsg ""  "Unknown type: Pair")
                end
              | Fst (_, e1) =>
                let
                    val e1 = aux e1
                in
                    case getType e1 of
                        SOME (Type.TyProduct (t1, t2)) => Fst (SOME t1, e1)
                      | _ => raise Fail (errorMsg ""  "Unknown type: Fst")
                end
              | Snd (t, e1) =>
                let
                    val e1 = aux e1
                in
                    case getType e1 of
                        SOME (Type.TyProduct (t1, t2)) => Snd (SOME t2, e1)
                      | _ => raise Fail (errorMsg ""  "Unknown type: Snd")
                end
              | Ifte (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME Type.TyBool, SOME t2, SOME t3) =>
                        if Type.eq (t2, t3)
                        then Ifte (SOME t2, e1, e2, e3)
                        else raise Fail (errorMsg "Ifte"  (conflictMsg t2 t3))
                      | _ => raise Fail (errorMsg "Ifte"  "Unknown type:")
                end
              | Con (t, c) => Con (t, c)
              | App (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyArrow (t11, t12)), SOME t2) =>
                        if Type.eq (t11, t2)
                        then App (SOME t12, e1, e2)
                        else raise Fail (errorMsg "App"  (conflictMsg t11 t2))
                      | _ => raise Fail (errorMsg "App"  "Unknown type")
                end
              | Abs(t, id, ty, e1) =>
                let
                    val e1 = aux e1
                in
                    case getType e1 of
                        SOME t1 => Abs (SOME (Type.TyArrow (ty, t1)), id, ty, e1)
                      | _ => raise Fail (errorMsg ""  "Unknown type: Abs")
                end
              | Op (t, oper, e1, e2) =>
                let
                    val inType =
                        case oper of
                            Operator.Add => Type.TyInt
                          | Operator.Mul => Type.TyInt
                          | Operator.Divi => Type.TyInt
                          | Operator.Less => Type.TyInt
                          | Operator.Eq => Type.TyInt
                          | Operator.Greater => Type.TyInt
                          | Operator.AddR => Type.TyReal
                          | Operator.MulR => Type.TyReal
                          | Operator.DiviR => Type.TyReal
                          | Operator.LessR => Type.TyReal
                          | Operator.EqB => Type.TyBool
                          | Operator.GreaterR => Type.TyReal
                    val outType =
                        case oper of
                            Operator.Add => Type.TyInt
                          | Operator.Mul => Type.TyInt
                          | Operator.Divi => Type.TyInt
                          | Operator.Less => Type.TyBool
                          | Operator.Eq => Type.TyBool
                          | Operator.Greater => Type.TyBool
                          | Operator.AddR => Type.TyReal
                          | Operator.MulR => Type.TyReal
                          | Operator.DiviR => Type.TyReal
                          | Operator.LessR => Type.TyBool
                          | Operator.EqB => Type.TyBool
                          | Operator.GreaterR => Type.TyBool
                    val e1 = aux e1
                    val e2 = aux e2
                    val (t1, t2) =
                        case (getType e1, getType e2) of
                            (SOME t1, SOME t2) => (t1, t2)
                          | _ => raise Fail (errorMsg "Op"  "Unknown type: Op")
                in
                    if (Type.eq (t1, inType))
                    then if (Type.eq (t2, inType))
                         then Op (SOME outType, oper, e1, e2)
                         else raise Fail (errorMsg "Op"  (conflictMsg t2 inType))
                    else raise Fail (errorMsg "Op"  (conflictMsg t1 inType))
                end
              | Map (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyArrow (t11, t12)), SOME (Type.TyList t2)) =>
                        if Type.eq (t11, t2)
                        then Map (SOME (Type.TyList t12), e1, e2)
                        else raise Fail (errorMsg "Map"  (conflictMsg t11 t2))
                      | _ => raise Fail (errorMsg "Map"  "Unknown type: Map")
                end
              | Foldl (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME (Type.TyArrow (Type.TyProduct (t11, t12), t13)), SOME t2, SOME (Type.TyList t3)) =>
                        if Type.eq (t12, t13)
                        then if Type.eq (t12, t2)
                             then if Type.eq (t11, t3)
                                  then Foldl (SOME t2, e1, e2, e3)
                                  else raise Fail (errorMsg "Foldl"  (conflictMsg t11 t3))
                             else raise Fail (errorMsg "Foldl"  (conflictMsg t12 t2))
                        else raise Fail (errorMsg "Foldl"  (conflictMsg t12 t13))
                      | _ => raise Fail (errorMsg "Foldl"  "Unknown type: Foldl")
                end
              | Mapi (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyArrow (Type.TyProduct (Type.TyInt, t11), t12)), SOME (Type.TyList t2)) =>
                        if Type.eq (t11, t2)
                        then Mapi (SOME (Type.TyList t12), e1, e2)
                        else raise Fail (errorMsg "Mapi"  (conflictMsg t11 t2))
                      | _ => raise Fail (errorMsg "Mapi"  "Unknown type: Mapi")
                end
              | Foldli (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME (Type.TyArrow (Type.TyProduct (Type.TyProduct (Type.TyInt, t11), t12), t13)), SOME t2, SOME (Type.TyList t3)) =>
                        if Type.eq (t12, t13)
                        then if Type.eq (t12, t2)
                             then if Type.eq (t11, t3)
                                  then Foldli (SOME t2, e1, e2, e3)
                                  else raise Fail (errorMsg "Foldli"  (conflictMsg t11 t3))
                             else raise Fail (errorMsg "Foldli"  (conflictMsg t12 t2))
                        else raise Fail (errorMsg "Foldli"  (conflictMsg t12 t13))
                      | _ => raise Fail (errorMsg "Foldli"  "Unknown type: Foldli")
                end
              | Nth (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyList t1), SOME Type.TyInt) => Nth (SOME t1, e1, e2)
                      | _ => raise Fail (errorMsg ""  "Unknown type: Nth")
                end
              | AMap (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyArrow (t11, t12)), SOME (Type.TyApprList t2)) =>
                        if Type.eq (t11, t2)
                        then AMap (SOME (Type.TyApprList t12), e1, e2)
                        else raise Fail (errorMsg "AMap"  (conflictMsg t11 t2))
                      | _ => raise Fail (errorMsg "AMap"  "Unknown type: AMap")
                end
              | AFoldl (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME (Type.TyArrow (Type.TyProduct (t11, t12), t13)), SOME t2, SOME (Type.TyApprList t3)) =>
                        if Type.eq (t12, t13)
                        then if Type.eq (t12, t2)
                             then if Type.eq (t11, t3)
                                  then AFoldl (SOME t2, e1, e2, e3)
                                  else raise Fail (errorMsg "AFoldl"  (conflictMsg t11 t3))
                             else raise Fail (errorMsg "AFoldl"  (conflictMsg t12 t2))
                        else raise Fail (errorMsg "AFoldl"  (conflictMsg t12 t13))
                      | _ => raise Fail (errorMsg "AFoldl"  "Unknown type: AFoldl")
                end
              | AMapi (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyArrow (Type.TyProduct (Type.TyInt, t11), t12)), SOME (Type.TyApprList t2)) =>
                        if Type.eq (t11, t2)
                        then AMapi (SOME (Type.TyApprList t12), e1, e2)
                        else raise Fail (errorMsg "AMapi"  (conflictMsg t11 t2))
                      | _ => raise Fail (errorMsg "AMapi"  "Unknown type: AMapi")
                end
              | AFoldli (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME (Type.TyArrow (Type.TyProduct (Type.TyProduct (Type.TyInt, t11), t12), t13)), SOME t2, SOME (Type.TyApprList t3)) =>
                        if Type.eq (t12, t13)
                        then if Type.eq (t12, t2)
                             then if Type.eq (t11, t3)
                                  then AFoldli (SOME t2, e1, e2, e3)
                                  else raise Fail (errorMsg "AFoldli"  (conflictMsg t11 t3))
                             else raise Fail (errorMsg "AFoldli"  (conflictMsg t12 t2))
                        else raise Fail (errorMsg "AFoldli"  (conflictMsg t12 t13))
                      | _ => raise Fail (errorMsg "AFoldli"  "Unknown type: AFoldli")
                end
              | ANth (t, e1, e2) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                in
                    case (getType e1, getType e2) of
                        (SOME (Type.TyApprList t1), SOME Type.TyInt) => ANth (SOME t1, e1, e2)
                      | _ => raise Fail (errorMsg ""  "Unknown type: ANth")
                end
              | Loop (t, e1, e2, e3) =>
                let
                    val e1 = aux e1
                    val e2 = aux e2
                    val e3 = aux e3
                in
                    case (getType e1, getType e2, getType e3) of
                        (SOME (Type.TyArrow (t11, t12)), SOME t2, SOME Type.TyInt) =>
                        if Type.eq (t11, t12)
                        then if Type.eq (t11, t2)
                             then Loop (SOME t2, e1, e2, e3)
                             else raise Fail (errorMsg "Loop"  (conflictMsg t11 t2))
                        else raise Fail (errorMsg "Loop"  (conflictMsg t11 t12))
                      | _ => raise Fail (errorMsg "Loop"  "Unknown type: Loop")
                end
    in
        aux ast
    end
end
