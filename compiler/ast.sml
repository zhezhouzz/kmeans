functor Ast (A : ATOM) =
struct
open A

datatype exp =
         Var of id
         | Pair of exp * exp
         | Fst of exp
         | Snd of exp
         | Ifte of exp * exp * exp
         | App of exp * exp
         | Abs of id * ctype * exp
         | AbsAppr of id * ctype * id * exp
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

  fun layout ast =
      case ast of
          Var v => v
        | Pair (e1, e2) =>
          let val s1 = layout e1
              val s2 = layout e2
          in
              "("^s1^","^s2^")"
          end
        | Fst e => "(fst " ^ (layout e) ^ ")"
        | Snd e => "(snd " ^ (layout e) ^ ")"
        | Ifte (e1, e2, e3) =>
          "(if " ^ (layout e1) ^ " then " ^ (layout e2) ^ " else " ^ (layout e3) ^ ")"
        | Con c => constToString c
        | App(e1,e2) =>
          let val s1 = layout e1
              val s2 = layout e2
          in
              "("^s1^" "^s2^")"
          end
        | Abs(id, t, e) =>
          let val s = layout e
          in
              "(fn ("^ id ^ " : " ^ (typeToString t) ^ ") => " ^ s ^")"
          end
        | AbsAppr(id, t, distr, e) =>
          let val s = layout e
          in
              "(fn ("^ id ^ " : " ^ (typeToString t) ^ " : " ^ distr ^ ") => " ^ s ^")"
          end
        | Op (oper,e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s0 = operToString oper
          in
              "(" ^ s1 ^ s0 ^ s2 ^ ")"
          end
        | Map (e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
          in
              "(map " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldl (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(foldl " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Mapi (e1, e2) =>
          let
              val s1 = layout e1
              val s2 = layout e2
          in
              "(mapi " ^ s1 ^ " " ^ s2 ^ ")"
          end
        | Foldli (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(foldli " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Nth (e1) =>
          let
              val s1 = layout e1
          in
              "(nth " ^ s1 ^ ")"
          end
        | Loop (e1, e2, e3) =>
          let
              val s1 = layout e1
              val s2 = layout e2
              val s3 = layout e3
          in
              "(loop " ^ s1 ^ " " ^ s2 ^ " " ^ s3 ^ ")"
          end
        | Unit => "()"

  fun astToSMLAux ast =
      case ast of
          Var x => x
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
        | Abs (id, _, e) => "(fn " ^ id ^ " => " ^ (astToSMLAux e) ^")"
        | AbsAppr (id, _, _, e) => "(fn " ^ id ^ " => " ^ (astToSMLAux e) ^")"
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

structure AstDsl = Ast(Atom);
