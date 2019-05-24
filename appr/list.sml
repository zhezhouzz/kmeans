signature APPRLIST =
sig
    structure A : APPRABLE
    type 'a t
    val sample : (A.t list -> (int * A.t) list) -> A.t list -> A.t t
    val length : A.t t -> int
    val samples: A.t t -> (int * A.t) list
    val build : (int * A.t) list -> int -> A.t t
    val nth : A.t t * int -> A.t
    val toList : A.t t -> A.t list
    val printInternal : A.t t -> unit
    val foldl : (A.t * 'b -> 'b) -> 'b -> A.t t -> 'b
    val foldli : ((int * A.t) * 'b -> 'b) -> 'b -> A.t t -> 'b
end

functor ApprList (structure A: APPRABLE) : APPRLIST =
struct
structure A = A
type 'a t = {samples: (int * 'a) list, len : int}
fun sample f l = {samples = f l, len = List.length l}
fun length {samples, len} = len
fun samples {samples, len} = samples
fun build samples len = {samples, len}
fun get (left, right) idx =
    case (left, right) of
        (NONE, NONE) => raise Fail "Get fail: no range at all."
      | (SOME (_, r), NONE) => r
      | (NONE, SOME (_, r)) => r
      | (SOME (idx1, r1), SOME (idx2, r2)) => A.linearAppr idx (idx1, r1) (idx2, r2)
fun nth ({samples, len}, i) =
    let
        val range =
            List.foldl (
                fn ((idx, r), (left, right)) =>
                   case (left, right) of
                       (NONE, NONE) =>
                       if idx > i
                       then (NONE, SOME (idx, r))
                       else (SOME (idx, r), NONE)
                     | (SOME (idx', _), NONE) =>
                       if idx > i
                       then (left, SOME (idx, r))
                       else (SOME (idx, r), NONE)
                     | _ => (left, right)
            ) (NONE, NONE) samples
        val v = get range i
    in
        v
    end
fun foldlPiece f default ((idx1, r1), (idx2, r2)) =
    let
        fun aux (i, r) =
            if i = idx2 then r else
            let
                val v = A.linearAppr i (idx1, r1) (idx2, r2)
                val r = f (v, r)
            in
                aux ((i + 1), r)
            end
    in
        aux (idx1, default)
    end
fun foldliPiece f default ((idx1, r1), (idx2, r2)) =
    let
        fun aux (i, r) =
            if i = idx2 then r else
            let
                val v = A.linearAppr i (idx1, r1) (idx2, r2)
                val r = f ((i, v), r)
            in
                aux ((i + 1), r)
            end
    in
        aux (idx1, default)
    end
fun foldl f default {samples, len} =
    case samples of
        [] => default
      | (idx1, r1) :: t =>
        let
            val (idx2, r2) = List.last samples
            val leftmost = (0, r1)
            val rightmost = (len, r2)
            val ranges = samples @ [rightmost]
            val (_, result) =
                List.foldl (fn (right, (left, r)) =>
                               (right, foldlPiece f r (left, right))
                           ) (leftmost, default) ranges
        in
            result
        end
fun foldli f default {samples, len} =
    case samples of
        [] => default
      | (idx1, r1) :: t =>
        let
            val (idx2, r2) = List.last samples
            val leftmost = (0, r1)
            val rightmost = (len, r2)
            val ranges = samples @ [rightmost]
            val (_, result) =
                List.foldl (fn (right, (left, r)) =>
                               (right, foldliPiece f r (left, right))
                           ) (leftmost, default) ranges
        in
            result
        end
fun toList {samples, len} =
    foldl (fn (e, r) => r @ [e]) [] {samples, len}

fun printInternal {samples, len} =
    let
        val _ = print ("len = " ^ (Int.toString len) ^ "\n")
        val _ = print ("APPR number: " ^ (Int.toString (List.length samples)) ^ "\n")
        val _ = print ((ExtendedList.toString (
                             fn (idx, e) => ((Int.toString idx) ^ ": " ^ (A.toString e) ^ "\n"),
                             samples)) ^ "\n")
    in
        ()
    end
end
