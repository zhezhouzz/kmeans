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
        Abs(t, id0, t0, e) =>
        let
            val e = fillAux e (id0, t0)
        in
            Abs(t, id0, t0, e)
        end
      | x => x

fun typeCheck ast =
    case fill ast of
        Var (t, id) =>
        (case t of
             NONE => raise Fail (errorMsg (Id.layout id)  "Unknown type")
          | SOME t => Var (SOME t, id)
        )
      | VarD (t, id, e) =>
        let
            val e = typeCheck e
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
              | _ => raise Fail (errorMsg (Id.layout id)  "Unknown type")
        end
      | Pair (_, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME t1, SOME t2) => Pair (SOME (Type.TyProduct (t1, t2)), e1, e2)
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Fst (_, e1) =>
        let
            val e1 = typeCheck e1
        in
            case getType e1 of
                SOME (Type.TyProduct (t1, t2)) => Fst (SOME t1, e1)
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Snd (t, e1) =>
        let
            val e1 = typeCheck e1
        in
            case getType e1 of
                SOME (Type.TyProduct (t1, t2)) => Snd (SOME t2, e1)
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Ifte (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME Type.TyBool, SOME t2, SOME t3) =>
                if Type.eq (t2, t3)
                then Ifte (SOME t2, e1, e2, e3)
                else raise Fail (errorMsg ""  (conflictMsg t2 t3))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Con (t, c) => Con (t, c)
      | App (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyArrow (t11, t12)), SOME t2) =>
                if Type.eq (t11, t2)
                then App (SOME t12, e1, e2)
                else raise Fail (errorMsg ""  (conflictMsg t11 t2))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Abs(t, id, ty, e1) =>
        let
            val e1 = typeCheck e1
        in
            case getType e1 of
                SOME t1 => Abs (SOME (Type.TyArrow (ty, t1)), id, ty, e1)
              | _ => raise Fail (errorMsg ""  "Unknown type")
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
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val (t1, t2) =
                case (getType e1, getType e2) of
                    (SOME t1, SOME t2) => (t1, t2)
                  | _ => raise Fail (errorMsg ""  "Unknown type")
        in
            if (Type.eq (t1, inType))
            then if (Type.eq (t2, inType))
                 then Op (SOME outType, oper, e1, e2)
                 else raise Fail (errorMsg ""  (conflictMsg t2 inType))
            else raise Fail (errorMsg ""  (conflictMsg t1 inType))
        end
      | Map (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyArrow (t11, t12)), SOME (Type.TyList t2)) =>
                if Type.eq (t11, t2)
                then Map (SOME (Type.TyList t12), e1, e2)
                else raise Fail (errorMsg ""  (conflictMsg t11 t2))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Foldl (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME (Type.TyArrow (Type.TyProduct (t11, t12), t13)), SOME t2, SOME (Type.TyList t3)) =>
                if Type.eq (t12, t13)
                then if Type.eq (t12, t2)
                     then if Type.eq (t11, t3)
                          then Foldl (SOME t2, e1, e2, e3)
                          else raise Fail (errorMsg ""  (conflictMsg t11 t3))
                     else raise Fail (errorMsg ""  (conflictMsg t12 t2))
                else raise Fail (errorMsg ""  (conflictMsg t12 t13))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Mapi (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyArrow (Type.TyProduct (Type.TyInt, t11), t12)), SOME (Type.TyList t2)) =>
                if Type.eq (t11, t2)
                then Mapi (SOME (Type.TyList t12), e1, e2)
                else raise Fail (errorMsg ""  (conflictMsg t11 t2))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Foldli (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME (Type.TyArrow (Type.TyProduct (Type.TyProduct (Type.TyInt, t11), t12), t13)), SOME t2, SOME (Type.TyList t3)) =>
                if Type.eq (t12, t13)
                then if Type.eq (t12, t2)
                     then if Type.eq (t11, t3)
                          then Foldli (SOME t2, e1, e2, e3)
                          else raise Fail (errorMsg ""  (conflictMsg t11 t3))
                     else raise Fail (errorMsg ""  (conflictMsg t12 t2))
                else raise Fail (errorMsg ""  (conflictMsg t12 t13))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Nth (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyList t1), SOME Type.TyInt) => Nth (SOME t1, e1, e2)
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | AMap (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyArrow (t11, t12)), SOME (Type.TyApprList t2)) =>
                if Type.eq (t11, t2)
                then AMap (SOME (Type.TyApprList t12), e1, e2)
                else raise Fail (errorMsg ""  (conflictMsg t11 t2))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | AFoldl (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME (Type.TyArrow (Type.TyProduct (t11, t12), t13)), SOME t2, SOME (Type.TyApprList t3)) =>
                if Type.eq (t12, t13)
                then if Type.eq (t12, t2)
                     then if Type.eq (t11, t3)
                          then AFoldl (SOME t2, e1, e2, e3)
                          else raise Fail (errorMsg ""  (conflictMsg t11 t3))
                     else raise Fail (errorMsg ""  (conflictMsg t12 t2))
                else raise Fail (errorMsg ""  (conflictMsg t12 t13))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | AMapi (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyArrow (Type.TyProduct (Type.TyInt, t11), t12)), SOME (Type.TyApprList t2)) =>
                if Type.eq (t11, t2)
                then AMapi (SOME (Type.TyApprList t12), e1, e2)
                else raise Fail (errorMsg ""  (conflictMsg t11 t2))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | AFoldli (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME (Type.TyArrow (Type.TyProduct (Type.TyProduct (Type.TyInt, t11), t12), t13)), SOME t2, SOME (Type.TyApprList t3)) =>
                if Type.eq (t12, t13)
                then if Type.eq (t12, t2)
                     then if Type.eq (t11, t3)
                          then AFoldli (SOME t2, e1, e2, e3)
                          else raise Fail (errorMsg ""  (conflictMsg t11 t3))
                     else raise Fail (errorMsg ""  (conflictMsg t12 t2))
                else raise Fail (errorMsg ""  (conflictMsg t12 t13))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | ANth (t, e1, e2) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
        in
            case (getType e1, getType e2) of
                (SOME (Type.TyApprList t1), SOME Type.TyInt) => ANth (SOME t1, e1, e2)
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
      | Loop (t, e1, e2, e3) =>
        let
            val e1 = typeCheck e1
            val e2 = typeCheck e2
            val e3 = typeCheck e3
        in
            case (getType e1, getType e2, getType e3) of
                (SOME (Type.TyArrow (t11, t12)), SOME t2, SOME Type.TyInt) =>
                if Type.eq (t11, t12)
                then if Type.eq (t11, t2)
                     then Loop (SOME t2, e1, e2, e3)
                     else raise Fail (errorMsg ""  (conflictMsg t11 t2))
                else raise Fail (errorMsg ""  (conflictMsg t11 t12))
              | _ => raise Fail (errorMsg ""  "Unknown type")
        end
end
