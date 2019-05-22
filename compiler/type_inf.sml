structure Counter =
struct
type t = int ref
fun init () = ref 0
fun cur c = !c
fun next c =
    let
        val _ = c := (!c + 1)
    in
        !c
    end
end

signature TYPE_INFERENCE =
sig
    structure DslAst : DSL_AST
    structure TypedAst : TYPED_AST
    sharing TypedAst.Atoms = DslAst.Atoms
    val inference : DslAst.top_level -> TypedAst.top_level
end

structure TypeInference =
struct
structure DslAst = DslAst
structure TypedAst = TypedAst
open TypedAst
open Atoms
fun addType counter ast =
    case ast of
            DslAst.Var id => Var (TmpVar (Counter.next counter), id)
          | DslAst.Pair (e1, e2) => Pair (TmpVar (Counter.next counter), addType counter e1, addType counter e2)
          | DslAst.Fst e => Fst (TmpVar (Counter.next counter), addType counter e)
          | DslAst.Snd e => Snd (TmpVar (Counter.next counter), addType counter e)
          | DslAst.Ifte (e1, e2, e3) => Ifte (TmpVar (Counter.next counter), addType counter e1, addType counter e2, addType counter e3)
          | DslAst.Con c =>
            (case c of
                 Const.ConstInt _ => Con (TmpInt, c)
               | Const.ConstReal _ => Con (TmpReal, c))
          | DslAst.App(e1,e2) => App (TmpVar (Counter.next counter), addType counter e1, addType counter e2)
          | DslAst.Abs(id, t, e) => Abs (TmpVar (Counter.next counter), id, t, addType counter e)
          | DslAst.Op (oper, e1, e2) => Op (TmpVar (Counter.next counter), oper, addType counter e1, addType counter e2)
          | DslAst.Map (e1, e2) => Map (TmpVar (Counter.next counter), addType counter e1, addType counter e2)
          | DslAst.Foldl (e1, e2, e3) => Foldl (TmpVar (Counter.next counter), addType counter e1, addType counter e2, addType counter e3)
          | DslAst.Mapi (e1, e2) => Mapi (TmpVar (Counter.next counter), addType counter e1, addType counter e2)
          | DslAst.Foldli (e1, e2, e3) => Foldli (TmpVar (Counter.next counter), addType counter e1, addType counter e2, addType counter e3)
          | DslAst.Nth e => Nth (TmpVar (Counter.next counter), addType counter e)
          | DslAst.Loop (e1, e2, e3) => Loop (TmpVar (Counter.next counter), addType counter e1, addType counter e2, addType counter e3)
          | DslAst.Unit => Unit TmpUnit

fun getType exp =
    case exp of
        Var (t, id) => t
     | Pair (t, e1, e2) => t
     | Fst (t, e) => t
     | Snd (t, e) => t
     | Ifte (t, e1, e2, e3) => t
     | Con (t, c) => t
     | App(t, e1, e2) => t
     | Abs(t, id, tDsl, e) => t
     | Op (t, oper, e1, e2) => t
     | Map (t, e1, e2) => t
     | Foldl (t, e1, e2, e3) => t
     | Mapi (t, e1, e2) => t
     | Foldli (t, e1, e2, e3) => t
     | Nth (t, e) => t
     | Loop (t, e1, e2, e3) => t
     | Unit t => t

type constraint = tmptype * tmptype

fun constraintLayout (t1, t2) =
    (tmptypeLayout t1) ^ " = " ^ (tmptypeLayout t2)
fun constraintsLayout l =
    List.foldl (fn (e, r) =>
                   r ^ "\n" ^ (constraintLayout e)
               ) "" l

fun substConstraints idx ty exp =
    case exp of
        Var (t, id) =>
        if Id.beq (id, idx)
        then
            [(ty, t)]
        else
            []
      | Pair (t, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Fst (t, e) => (substConstraints idx ty e)
      | Snd (t, e) => (substConstraints idx ty e)
      | Ifte (t, e1, e2, e3) => (substConstraints idx ty e1) @ (substConstraints idx ty e2) @ (substConstraints idx ty e3)
      | App(t, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Abs(t, id, tDsl, e) =>
        if Id.beq (id, idx)
        then
            []
        else
            (substConstraints idx ty e)
      | Op (t, oper, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Map (t, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Foldl (t, e1, e2, e3) => (substConstraints idx ty e1) @ (substConstraints idx ty e2) @ (substConstraints idx ty e3)
      | Mapi (t, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Foldli (t, e1, e2, e3) => (substConstraints idx ty e1) @ (substConstraints idx ty e2) @ (substConstraints idx ty e3)
      | Nth (t, e) => (substConstraints idx ty e)
      | Loop (t, e1, e2, e3) => (substConstraints idx ty e1) @ (substConstraints idx ty e2) @ (substConstraints idx ty e3)
      | _ => []

fun dslTypeToTmpType t =
    case t of
        Type.TyInt => SOME TmpInt
      | Type.TyReal => SOME TmpInt
      | Type.TyList t =>
        (case dslTypeToTmpType t of
            NONE => NONE
          | SOME t => SOME (TmpList t))
      | Type.TyArrow (t1, t2) =>
        (case (dslTypeToTmpType t1, dslTypeToTmpType t2) of
             (SOME t1, SOME t2) => SOME (TmpArrow (t1, t2))
          | _ => NONE
        )
      | Type.TyPair (t1, t2) =>
        (case (dslTypeToTmpType t1, dslTypeToTmpType t2) of
             (SOME t1, SOME t2) => SOME (TmpProduct (t1, t2))
           | _ => NONE
        )
      | Type.TyUnit => SOME TmpUnit
      | Type.TyUnknown => NONE


fun getConstraints counter ast =
    case ast of
        Var (t, id) => []
      | Pair (t, e1, e2) => (t, TmpProduct (getType e1, getType e2)) :: (getConstraints counter e1) @ (getConstraints counter e2)
      | Fst (t, e) =>
        let
            val t' = getType e
            val t2 = TmpVar (Counter.next counter)
        in
            (t', TmpProduct (t, t2)) :: (getConstraints counter e)
        end
      | Snd (t, e) =>
        let
            val t' = getType e
            val t1 = TmpVar (Counter.next counter)
        in
            (t', TmpProduct (t1, t)) :: (getConstraints counter e)
        end
      | Ifte (t, e1, e2, e3) =>
        [(t, getType e1), (t, getType e2), (t, getType e3)] @
        (getConstraints counter e1) @
        (getConstraints counter e2) @
        (getConstraints counter e3)
      | Con (t, c) => []
      | App(t, e1, e2) => (t, TmpArrow (getType e1, getType e2)) :: (getConstraints counter e1) @ (getConstraints counter e2)
      | Abs(t, id, tDsl, e) =>
        let
            val spec =
                case dslTypeToTmpType tDsl of
                    NONE => TmpVar (Counter.next counter)
                  | SOME t' => t'
            val cons1 = substConstraints id spec e
            (* val _ = print "substConstraints:\n" *)
            (* val _ = print (constraintsLayout cons1) *)
            (* val _ = print "\n" *)
            val cons2 = getConstraints counter e
        in
            (TmpArrow (spec, getType e), t) :: cons1 @ cons2
        end
      | Op (t, oper, e1, e2) =>
        [(t, getType e1), (t, getType e2)] @
        (getConstraints counter e1) @
        (getConstraints counter e2)
      | Map (t, e1, e2) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val tyB = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (tyA, tyB)
            val ty2 = TmpList tyA
            val ty0 = TmpList tyB
        in
            [(ty1, getType e1), (ty2, getType e2), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2)
        end
      | Foldl (t, e1, e2, e3) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val tyB = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (TmpProduct (tyA, tyB), tyB)
            val ty2 = tyB
            val ty3 = TmpList tyA
            val ty0 = tyB
        in
            [(ty1, getType e1), (ty2, getType e2), (ty3, getType e3), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2) @
            (getConstraints counter e3)
        end
      | Mapi (t, e1, e2) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val tyB = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (TmpProduct (TmpInt, tyA), tyB)
            val ty2 = TmpList tyA
            val ty0 = TmpList tyB
        in
            [(ty1, getType e1), (ty2, getType e2), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2)
        end
      | Foldli (t, e1, e2, e3) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val tyB = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (TmpProduct (TmpProduct (TmpInt, tyA), tyB), tyB)
            val ty2 = tyB
            val ty3 = TmpList tyA
            val ty0 = tyB
        in
            [(ty1, getType e1), (ty2, getType e2), (ty3, getType e3), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2) @
            (getConstraints counter e3)
        end
      | Nth (t, e) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val ty1 = TmpList tyA
            val ty0 = tyA
        in
            [(ty1, getType e), (t, ty0)] @ (getConstraints counter e)
        end
      | Loop (t, e1, e2, e3) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (tyA, tyA)
            val ty2 = TmpInt
            val ty3 = tyA
            val ty0 = tyA
        in
            [(ty1, getType e1), (ty2, getType e2), (ty3, getType e3), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2) @
            (getConstraints counter e3)
        end
      | Unit t => []

fun constraints ast =
    let
        val c = Counter.init ()
        val ast = addType c ast
        val _ = print ((TypedAst.layout ast) ^ "\n")
        val cons = getConstraints c ast
        val _ = print (constraintsLayout cons)
    in
        ()
    end
end
