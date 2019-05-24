fun fst (x, y) = x
fun snd (x, y) = y

(* Additional functions for List *)
signature EXTENDED_LIST =
sig
    val mapi: (int * 'a -> 'b) * 'a list -> 'b list
    val foldli: ('b * int * 'a  -> 'b) * 'b * 'a list -> 'b
    val toString: ('a -> string) * 'a list -> string
    val bestOpt: ('a * 'a -> bool) * 'a list -> 'a option
    val subtract: ('a * 'a -> bool) * 'a list * 'a list -> 'a list
    val bestDefault: ('a * 'a -> bool) * 'a * 'a list -> 'a
end

structure ExtendedList : EXTENDED_LIST =
struct

fun mapi (f, l) =
    let
        fun aux l i =
            case l of [] => []
                    | h :: t =>
                      (f (i, h)) :: (aux t (i + 1))
    in
        aux l 0
    end

fun foldli (f, r, l) =
    let
        fun aux r l i =
            case l of [] => r
                    | h :: t =>
                      aux (f (r, i, h)) t (i+1)
    in
        aux r l 0
    end

fun toString (f, l) =
    case l of [] => ""
            | h::t =>
              ((f h) ^ ", ") ^ (toString (f, t))

fun bestOpt (compare, l) =
    case l of
        [] => NONE
      | h :: t =>
        SOME (List.foldl (fn (e, r) =>
                             if compare (e, r)
                             then
                                 e
                             else
                                 r
                         ) h t)

fun subtract (eqb, l1, l2) =
    let
        fun listIn l i =
            case l of
                [] => false
              | h::t => if eqb (i, h) then true else (listIn t i)
    in
        List.foldl (fn (e, r) => if listIn l2 e then r else e::r) [] l1
    end

fun bestDefault (f, default, l) =
    List.foldl (fn (e, r) => if f (e, r) then e else r) default l
end


structure Mlrandom =
struct

exception BAD_ERROR

fun init () =
    case (MLton.Random.seed ()) of NONE => raise BAD_ERROR
                                 | SOME w => MLton.Random.srand w
fun rand () = Word.toInt (MLton.Random.rand ())
fun uniformInt (a, b) =
    let
        val diff = Word.fromInt (b - a)
        val r = MLton.Random.rand ()
    in
        a + (Word.toInt (Word.mod (r, diff)))
    end
fun uniformReal (a, b) =
    let
        val diff = b - a
        val r = uniformInt (0, 10000)
    in
        a + (Real.fromInt r) * diff / 10000.0
    end
end;
