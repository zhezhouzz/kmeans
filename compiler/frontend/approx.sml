signature APPROX =
sig
    structure FallBack : FALL_BACK
    include FALL_BACK
    structure ApprAst : APPR_AST
    sharing ApprAst.Atoms = TypedAst.Atoms
    sharing ApprAst.ApprKernel.Type = TypedAst.Type
    val approx : TypedAst.top_level -> ApprAst.top_level
end

functor Approx (FallBack : FALL_BACK) : APPROX =
struct
structure FallBack = FallBack
open FallBack
structure ApprAst = ApprAst(structure Atoms = TypedAst.Atoms
                            structure Type = TypedAst.Type)
structure T = TypedAst.Type
structure A = TypedAst.Atoms
structure K = ApprAst.ApprKernel

fun sampleFunction exp =
    let
        val t = TypedAst.getType exp
    in
        case t of
            T.TyArrow (T.TyList (T.TyProduct (T.TyReal, T.TyReal)), T.TyReal) =>
            ApprAst.Var (T.TyArrow (
                              T.TyList (T.TyProduct (T.TyReal, T.TyReal)),
                              T.TyList (T.TyProduct (T.TyInt, (T.TyProduct (T.TyReal, T.TyReal))))
                          ),
                         A.Id.parse "fullSample")
          | T.TyArrow (T.TyList T.TyInt, T.TyReal) =>
            ApprAst.Var (T.TyArrow (
                              T.TyList T.TyInt,
                              T.TyList (T.TyProduct (T.TyInt, T.TyInt))
                          ),
                         A.Id.parse "fullSample")
          | _ => raise Fail "No such sampling function."
    end

fun approx ast =
    case ast of
        TypedAst.Var (t, v) => ApprAst.Var (t, v)
      | TypedAst.VarD (t, v, e1) =>
        (case t of
             T.TyApprList t =>
             ApprAst.ASample (T.TyApprList t, K.ASample (T.TyApprList t), T.TyList t, v, sampleFunction e1)
           | _ => raise Fail "Bad Error."
        )
      | TypedAst.Pair (t, e1, e2) => ApprAst.Pair (t, approx e1, approx e2)
      | TypedAst.Fst (t, e1) => ApprAst.Fst (t, approx e1)
      | TypedAst.Snd (t, e1) => ApprAst.Snd (t, approx e1)
      | TypedAst.Ifte (t, e1, e2, e3) =>
        ApprAst.Ifte (t, approx e1, approx e2, approx e3)
      | TypedAst.Con (t, c) => ApprAst.Con (t, c)
      | TypedAst.App (t, e1, e2) => ApprAst.App (t, approx e1, approx e2)
      | TypedAst.Abs(t, id, ty, e1) => ApprAst.Abs (t, id, ty, approx e1)
      | TypedAst.Op (t, oper, e1, e2) => ApprAst.Op (t, oper, approx e1, approx e2)
      | TypedAst.Map (t, e1, e2) => ApprAst.Map (t, approx e1, approx e2)
      | TypedAst.Foldl (t, e1, e2, e3) => ApprAst.Foldl (t, approx e1, approx e2, approx e3)
      | TypedAst.Mapi (t, e1, e2) => ApprAst.Mapi (t, approx e1, approx e2)
      | TypedAst.Foldli (t, e1, e2, e3) => ApprAst.Foldli (t, approx e1, approx e2, approx e3)
      | TypedAst.Nth (t, e1, e2) => ApprAst.Nth (t, approx e1, approx e2)
      | TypedAst.AMap (t, e1, e2) =>
        ApprAst.AMap (t, K.AMap (t, TypedAst.getType e2), approx e1, approx e2)
      | TypedAst.AFoldl (t, e1, e2, e3) =>
        ApprAst.AFoldl (t, K.AFoldl (TypedAst.getType e3), approx e1, approx e2, approx e3)
      | TypedAst.AMapi (t, e1, e2) =>
        ApprAst.AMapi (t, K.AMapi (t, TypedAst.getType e2), approx e1, approx e2)
      | TypedAst.AFoldli (t, e1, e2, e3) =>
        ApprAst.AFoldli (t, K.AFoldli (TypedAst.getType e3), approx e1, approx e2, approx e3)
      | TypedAst.ANth (t, e1, e2) =>
        ApprAst.ANth (t, K.ANth (TypedAst.getType e1), approx e1, approx e2)
      | TypedAst.Loop (t, e1, e2, e3) => ApprAst.Loop (t, approx e1, approx e2, approx e3)
end

