signature FALL_BACK =
sig
    structure Simplify : SIMPLIFY
    include SIMPLIFY
    val fallback : TypedAst.top_level -> string
end

functor FallBack (Simplify : SIMPLIFY) : FALL_BACK =
struct
structure Simplify = Simplify
open Simplify
open TypedAst
open Format
structure T = Type
fun typeFallback t =
    case t of
        T.TyApprList x => T.TyList x
      | x => x
fun fallback ast =
    let
        fun aux ast =
            case ast of
                Var (t, v) => Var (typeFallback t, v)
              | VarD (t, v, e) => Var (typeFallback t, v)
              | Pair (t, e1, e2) => Pair (typeFallback t, aux e1, aux e2)
              | Fst (t, e1) => Fst (typeFallback t, aux e1)
              | Snd (t, e1) => Snd (typeFallback t, aux e1)
              | Ifte (t, e1, e2, e3) => Ifte (typeFallback t, aux e1, aux e2, aux e3)
              | Con (t, c) => Con (typeFallback t, c)
              | App (t, e1, e2) => App (typeFallback t, aux e1, aux e2)
              | Abs(t, id, ty, e1) => Abs(typeFallback t, id, typeFallback ty, aux e1)
              | Op (t, oper, e1, e2) => Op (typeFallback t, oper, aux e1, aux e2)
              | Map (t, e1, e2) =>  Map (typeFallback t, aux e1, aux e2)
              | Foldl (t, e1, e2, e3) =>  Foldl (typeFallback t, aux e1, aux e2, aux e3)
              | Mapi (t, e1, e2) => Mapi (typeFallback t, aux e1, aux e2)
              | Foldli (t, e1, e2, e3) => Foldli (typeFallback t, aux e1, aux e2, aux e3)
              | Nth (t, e1, e2) => Nth (typeFallback t, aux e1, aux e2)
              | AMap (t, e1, e2) => Map (typeFallback t, aux e1, aux e2)
              | AFoldl (t, e1, e2, e3) => Foldl (typeFallback t, aux e1, aux e2, aux e3)
              | AMapi (t, e1, e2) => Mapi (typeFallback t, aux e1, aux e2)
              | AFoldli (t, e1, e2, e3) => Foldli (typeFallback t, aux e1, aux e2, aux e3)
              | ANth (t, e1, e2) => Nth (typeFallback t, aux e1, aux e2)
              | Loop (t, e1, e2, e3) => Loop (typeFallback t, aux e1, aux e2, aux e3)
    in
        (layout (aux ast)) ^ ";"
    end
end
