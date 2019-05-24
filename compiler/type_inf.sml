structure Counter =
struct
type t = int ref
fun init () = ref 0
fun cur c = !c
fun next c =
    let
        val n = !c
        val _ = c := (!c + 1)
    in
        n
    end
end

signature TYPE_INFERENCE =
sig
    structure DslAst : DSL_AST
    structure TypedAst : TYPED_AST
    structure TypeTrans : TYPE_TRANS
    sharing TypedAst.Atoms = DslAst.Atoms
    sharing TypeTrans.Type = DslAst.Atoms.Type
    sharing TypedAst.TmpType = TypedAst.TmpType
    val inference : DslAst.top_level -> TypedAst.top_level
end

structure TypeInference =
struct
structure DslAst = DslAst
structure TypedAst = TypedAst
structure TypeTrans = TypeTrans
open TypedAst
open Atoms

(* fun TypeTrans.trans t = *)
(*     case t of *)
(*         Type.TyInt => SOME TmpInt *)
(*       | Type.TyReal => SOME TmpReal *)
(*       | Type.TyBool => SOME TmpBool *)
(*       | Type.TyList t => *)
(*         (case TypeTrans.trans t of *)
(*              NONE => NONE *)
(*            | SOME t => SOME (TmpList t)) *)
(*       | Type.TyArrow (t1, t2) => *)
(*         (case (TypeTrans.trans t1, TypeTrans.trans t2) of *)
(*              (SOME t1, SOME t2) => SOME (TmpArrow (t1, t2)) *)
(*            | _ => NONE *)
(*         ) *)
(*       | Type.TyPair (t1, t2) => *)
(*         (case (TypeTrans.trans t1, TypeTrans.trans t2) of *)
(*              (SOME t1, SOME t2) => SOME (TmpProduct (t1, t2)) *)
(*            | _ => NONE *)
(*         ) *)
(*       | Type.TyUnit => SOME TmpUnit *)
(*       | Type.TyUnknown => NONE *)
open TmpType.TmpTypeStructure
fun addType counter ast =
    case ast of
        DslAst.Var id => Var (TmpVar (Counter.next counter), id)
      | DslAst.ImportedVar (id, t) =>
        (case TypeTrans.trans t of
             NONE => raise Fail "import variable should be typed!"
           | SOME t' => ImportedVar (t', id, t))
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
      | DslAst.Nth (e1, e2) => Nth (TmpVar (Counter.next counter), addType counter e1, addType counter e2)
      | DslAst.Loop (e1, e2, e3) => Loop (TmpVar (Counter.next counter), addType counter e1, addType counter e2, addType counter e3)
      | DslAst.Unit => Unit TmpUnit
      | DslAst.True => True TmpBool
      | DslAst.False => False TmpBool

fun getType exp =
    case exp of
        Var (t, id) => t
      | ImportedVar (t, id, tDsl) => t
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
      | Nth (t, e1, e2) => t
      | Loop (t, e1, e2, e3) => t
      | Unit t => t
      | True t => t
      | False t => t

type constraint = TmpType.t * TmpType.t

fun constraintLayout (t1, t2) =
    (TmpType.layout t1) ^ " = " ^ (TmpType.layout t2)
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
      | ImportedVar (t, id, tDsl) =>
        if Id.beq (id, idx)
        then
            (print "Warning: using the same variable name with an imported variable.\n"; [])
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
      | Nth (t, e1, e2) => (substConstraints idx ty e1) @ (substConstraints idx ty e2)
      | Loop (t, e1, e2, e3) => (substConstraints idx ty e1) @ (substConstraints idx ty e2) @ (substConstraints idx ty e3)
      | _ => []

fun getConstraints counter ast =
    case ast of
        Var (t, id) => []
      | ImportedVar (t, id, tDsl) => []
      | Pair (t, e1, e2) =>
        [(t, TmpProduct (getType e1, getType e2))] @
        (getConstraints counter e1) @
        (getConstraints counter e2)
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
        [(getType e1, TmpBool), (getType e2, getType e3), (t, getType e3)] @
        (getConstraints counter e1) @
        (getConstraints counter e2) @
        (getConstraints counter e3)
      | Con (t, c) => []
      | App(t, e1, e2) =>
        (getType e1, TmpArrow (getType e2, t)) :: (getConstraints counter e1) @ (getConstraints counter e2)
      | Abs(t, id, tDsl, e) =>
        let
            val spec =
                case TypeTrans.trans tDsl of
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
        let
            val cons =
                case oper of
                    Operator.Add => [(t, getType e1), (t, getType e2)]
                  | Operator.Mul => [(t, getType e1), (t, getType e2)]
                  | Operator.Divi => [(t, getType e1), (t, getType e2)]
                  | Operator.Less => [(t, TmpBool), (getType e1, getType e2)]
                  | Operator.Eq => [(t, TmpBool), (getType e1, getType e2)]
                  | Operator.Greater => [(t, TmpBool), (getType e1, getType e2)]
        in
            cons @ (getConstraints counter e1) @ (getConstraints counter e2)
        end
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
      | Nth (t, e1, e2) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val ty1 = TmpList tyA
            val ty2 = TmpInt
            val ty0 = tyA
        in
            [(ty1, getType e1), (ty2, getType e2), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2)
        end
      | Loop (t, e1, e2, e3) =>
        let
            val tyA = TmpVar (Counter.next counter)
            val ty1 = TmpArrow (tyA, tyA)
            val ty2 = tyA
            val ty3 = TmpInt
            val ty0 = tyA
        in
            [(ty1, getType e1), (ty2, getType e2), (ty3, getType e3), (t, ty0)] @
            (getConstraints counter e1) @
            (getConstraints counter e2) @
            (getConstraints counter e3)
        end
      | Unit t => []
      | True t => []
      | False t => []

fun unificateType t table =
    case t of
        TmpVar i =>
        (case Array.sub (table, i) of
             TmpVar j =>
             if i = j
             then TmpVar i
             else if j > i
             then raise Fail "BAD type unification."
             else unificateType (TmpVar j) table
           | x => unificateType x table)
      | TmpInt => TmpInt
      | TmpReal => TmpReal
      | TmpUnit => TmpUnit
      | TmpBool => TmpBool
      | TmpProduct (t1, t2) =>
        TmpProduct (unificateType t1 table, unificateType t2 table)
      | TmpArrow (t1, t2) =>
        TmpArrow (unificateType t1 table, unificateType t2 table)
      | TmpList t => TmpList (unificateType t table)

fun constraintsSolver cons n =
    let
        val table = Array.tabulate (n, fn i => TmpVar i)
        fun solveOne (t1, t2) =
            let
                val (t1, t2) = (unificateType t1 table, unificateType t2 table)
            in
                if TmpType.eq (t1, t2) then [] else
                case (t1, t2) of
                    (TmpVar i, TmpVar j) =>
                    if i < j then
                        (Array.update (table, j, (Array.sub (table, i))); [])
                    else
                        (Array.update (table, i, (Array.sub (table, j))); [])
                  | (TmpVar i, _) =>
                    if TmpType.fvIn t2 i
                    then
                        raise Fail ("Can't resolve constraint: " ^ (constraintLayout (t1, t2)))
                    else
                        (Array.update (table, i, t2); [])
                  | (_, TmpVar j) =>
                    if TmpType.fvIn t1 j
                    then
                        raise Fail ("Can't resolve constraint: " ^ (constraintLayout (t1, t2)))
                    else
                        (Array.update (table, j, t1); [])
                  | (TmpInt, TmpInt) => []
                  | (TmpReal, TmpReal) => []
                  | (TmpUnit, TmpUnit) => []
                  | (TmpBool, TmpBool) => []
                  | (TmpProduct (t11, t12), TmpProduct (t21, t22)) => [(t11, t21), (t12, t22)]
                  | (TmpArrow (t11, t12), TmpArrow (t21, t22)) => [(t11, t21), (t12, t22)]
                  | (TmpList t1, TmpList t2) => [(t1, t2)]
                  | _ => raise Fail ("Can't resolve constraint: " ^ (constraintLayout (t1, t2)))
            end
        fun pass cons =
            List.foldl (fn (e, r) =>
                           r @ (solveOne e)
                       ) [] cons
        fun aux cons =
            case cons of
                [] => ()
              | _ => aux (pass cons)
        val _ = aux cons
    in
        table
    end

fun unificateTable table =
    Array.foldli (fn (i, t, _) => Array.update (table, i, unificateType t table)) () table

fun unificateAst ast table =
    case ast of
        Var (t, id) => Var (unificateType t table, id)
      | ImportedVar (t, id, tDsl) => ImportedVar (t, id, tDsl)
      | Pair (t, e1, e2) => Pair (unificateType t table, unificateAst e1 table, unificateAst e2 table)
      | Fst (t, e) => Fst (unificateType t table, unificateAst e table)
      | Snd (t, e) => Snd (unificateType t table, unificateAst e table)
      | Ifte (t, e1, e2, e3) =>
        Ifte (unificateType t table,
              unificateAst e1 table,
              unificateAst e2 table,
              unificateAst e3 table)
      | Con (t, c) => Con (unificateType t table, c)
      | App(t, e1, e2) => App (unificateType t table, unificateAst e1 table, unificateAst e2 table)
      | Abs(t, id, tDsl, e) => Abs (unificateType t table, id, tDsl, unificateAst e table)
      | Op (t, oper, e1, e2) => Op (unificateType t table, oper, unificateAst e1 table, unificateAst e2 table)
      | Map (t, e1, e2) => Map (unificateType t table, unificateAst e1 table, unificateAst e2 table)
      | Foldl (t, e1, e2, e3) =>
        Foldl (unificateType t table,
               unificateAst e1 table,
               unificateAst e2 table,
               unificateAst e3 table)
      | Mapi (t, e1, e2) => Mapi (unificateType t table, unificateAst e1 table, unificateAst e2 table)
      | Foldli (t, e1, e2, e3) =>
        Foldli (unificateType t table,
                unificateAst e1 table,
                unificateAst e2 table,
                unificateAst e3 table)
      | Nth (t, e1, e2) => Nth (unificateType t table, unificateAst e1 table, unificateAst e2 table)
      | Loop (t, e1, e2, e3) =>
        Loop (unificateType t table,
              unificateAst e1 table,
              unificateAst e2 table,
              unificateAst e3 table)
      | Unit t => Unit (unificateType t table)
      | True t => True (unificateType t table)
      | False t => False (unificateType t table)

fun tableLayout table =
    Array.foldli (fn (i, t, r) =>
                     r ^ "\n" ^ (constraintLayout (TmpVar i, t))
                 ) "" table

fun inference ast =
    let
        val c = Counter.init ()
        val ast = addType c ast
        val _ = print ((TypedAst.layout ast) ^ "\n")
        val cons = getConstraints c ast
        (* val _ = print ((constraintsLayout cons) ^ "\n") *)
        val n = Counter.cur c
        (* val _ = print (" n = " ^ (Int.toString n) ^ "\n") *)
        val table = constraintsSolver cons n
        (* val _ = print ((tableLayout table) ^ "\n") *)
        val _ = unificateTable table
        (* val _ = print ((tableLayout table) ^ "\n") *)
        val ast = unificateAst ast table
        val _ = print ((TypedAst.layout ast) ^ "\n")
    in
        ()
    end
end
