structure Ast =
struct
  type id = string

  datatype const =
           ConstInt of int
           | ConstReal of real
  datatype oper =
           Add
           | Mul
           | Less
           | Eq
           | Greater
  datatype exp =
           Var of id
           | App of exp*exp
           | Abs of id*exp
           | Con of const
           | Op of oper * exp * exp
           | Map of exp * exp
           | Foldl of exp * exp * exp

  type top_level = exp

  fun constFromString str =
      let
          val _ = print ("constFromString : " ^ str ^ "\n")
      in
          case Real.fromString str of
              SOME x => Con (ConstReal x)
            | NONE =>
              case Int.fromString str of
                  SOME x => Con (ConstInt x)
                | NONE => raise Fail ("constFromString fail: " ^ str)
      end

  fun constToString c =
      case c of
          ConstInt x => Int.toString x
        | ConstReal x => Real.toString x

  fun operToString oper =
      case oper of
          Add => "+"
        | Mul => "*"
        | Less => "<"
        | Eq => "="
        | Greater => ">"

  fun layout ast =
      case ast of
          Var id => id
        | Con c => constToString c
        | App(e1,e2) =>
      let val s1 = layout e1
          val s2 = layout e2
      in
        "("^s1^" "^s2^")"
      end
    | Abs(id,e) =>
      let val s = layout e
      in
        "(fn "^id^" => " ^ s ^")"
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
  (* (* *)
  (*   freeVars returns list of free variables in given term *)
  (* *) *)
  (* fun freeVars e = case e of *)
  (*     Var id => [id] *)
  (*   | App (e1,e2) => List.concat [freeVars e1,freeVars e2] *)
  (*   | Abs (id,e') => List.filter (fn fv => not (fv = id)) (freeVars e') *)

  (* (*  *)
  (*   Alpha renaming - Renames bound var id as id' such that *)
  (*   1. There is no free var in e' named id' *)
  (*   2. Any sub-term in e' with bound var name as id' is recursively  *)
  (*      alpha-converted. This is achieved by calling capture-avoiding *)
  (*      subst on e'. *)
  (* *) *)
  (* fun alphaConvert e = case e of *)
  (*     Abs (id,e') =>  *)
  (*     let *)
  (*       val fv_e' = freeVars e' *)
  (*       (* Prove that this terminates *) *)
  (*       fun createNewName fvs hint =  *)
  (*         if List.exists (fn fv => fv = hint) fvs *)
  (*         then createNewName fvs (hint^"'") *)
  (*         else hint *)
  (*       val id' = createNewName fv_e' (id^"'") *)
  (*     in *)
  (*       Abs(id',subst(Var id',id,e')) *)
  (*     end *)
  (*   | _ => raise Fail "No alpha-conversion for Unbound terms" *)

  (* (* Capture-avoiding substitution *) *)
  (* and subst(e1,id,e2) = case e2 of *)
  (*     Var id' => if id = id'  *)
  (*       then e1 else e2 *)
  (*   | App(e21,e22) => App(subst(e1,id,e21),subst(e1,id,e22)) *)
  (*   | Abs(id',e2') => if id' = id then e2 else *)
  (*     let *)
  (*       val fv_e1 = freeVars(e1) *)
  (*     in *)
  (*       if List.exists (fn fv => fv = id') fv_e1 *)
  (*       then subst(e1,id,alphaConvert e2) *)
  (*       else Abs(id',subst(e1,id,e2')) *)
  (*     end *)
end
