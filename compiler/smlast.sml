functor SmlAst (A : AST) =
struct
type id = string

type const = A.const
type oper = A.const
type ctype = A.ctype
datatype exp =
         Var of id
         | Pair of exp * exp
         | Fst of exp
         | Snd of exp
         | Ifte of exp * exp * exp
         | App of exp * exp
         | Abs of id * ctype * exp
         | Con of const
         | Op of oper * exp * exp
         | Map of exp * exp
         | Foldl of exp * exp * exp
         | Mapi of exp * exp
         | Foldli of exp * exp * exp
         | Nth of exp
         | Loop of exp * exp * exp
       | Unit

type top_level = exp

fun appr ast =
    case ast of
        A.AbsAppr(id, t, distr, e) =>
        Abs (id, t, App (Abs (id, t, appr e), App (Var distr, Var id)))
      | A.Var v => Var v
      | A.Pair (e1, e2) => A.Pair (appr e1, appr e2)
      | A.Fst e => Fst (appr e)
      | A.Snd e => Snd (appr e)
      | A.Ifte (e1, e2, e3) => Ifte (appr e1, appr e2, appr e3)
      | A.Con c => Con c
      | A.App (e1, e2) => App (appr e1, appr e2)
      | A.Abs (v, e) => Abs (v, appr e)
      | A.Op (oper, e1, e2) => Op (oper, appr e1, appr e2)
      | A.Map (e1, e2) => Map (appr e1, appr e2)
      | A.Foldl (e1, e2, e3) => Foldl (appr e1, appr e2, appr e3)
      | A.Mapi (e1, e2) => Mapi (appr e1, appr e2)
      | A.Foldli (e1, e2, e3) => Foldli (appr e1, appr e2, appr e3)
      | A.Nth (e) => Nth (appr e)
      | A.Loop (e1, e2, e3) => Loop (appr e1, appr e2, appr e3)
      | A.Unit => Unit

  fun astToSMLAux ast =
      case ast of
          VarType (Cvar (x, _)) => x
        | Pair (e1, e2) =>
          let val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
          in
              "("^s1^","^s2^")"
          end
        | Fst e => "(fst " ^ (astToSMLAux e) ^ ")"
        | Snd e => "(snd " ^ (astToSMLAux e) ^ ")"
        | Ifte (e1, e2, e3) =>
          "(if " ^ (astToSMLAux e1) ^ " then " ^ (astToSMLAux e2) ^ " else " ^ (astToSMLAux e3) ^ ")"
        | Con c => constToString c
        | App (e1, e2) => "("^ (astToSMLAux e1) ^ " " ^ (astToSMLAux e2) ^ ")"
        | Abs (Cvar (id, _), e) => "(fn " ^ id ^ " => " ^ (astToSMLAux e) ^")"
        | Op (oper,e1, e2) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
              val s0 = operToString oper
          in
              "(" ^ s1 ^ s0 ^ s2 ^ ")"
          end
        | Map (e1, e2) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
          in
              "(ExtendedList.map " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldl (e1, e2, e3) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
              val s3 = astToSMLAux e3
          in
              "(ExtendedList.foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Mapi (e1, e2) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
          in
              "(ExtendedList.mapi " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldli (e1, e2, e3) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
              val s3 = astToSMLAux e3
          in
              "(ExtendedList.foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Nth (e1) =>
          let
              val s1 = astToSMLAux e1
          in
              "(ExtendedList.nth " ^ s1  ^ ")"
          end
        | Loop (e1, e2, e3) =>
          let
              val s1 = astToSMLAux e1
              val s2 = astToSMLAux e2
              val s3 = astToSMLAux e3
          in
              "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Unit => "()"

  fun astToSML ast =
      "val _ = " ^ (astToSMLAux ast) ^ ";\n"

end
