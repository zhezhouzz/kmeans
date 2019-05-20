signature A_IMP =
sig
    type spec
    val a1 : spec -> string
    val a2 : string -> spec
end

signature A =
sig
    type spec
    val a1 : spec -> string
    val a2 : string -> spec
end

structure AImp =
struct
type spec = string
fun a1 x = x
fun a2 x = x
end

signature B =
sig
    structure A : A
    datatype new = New of A.spec
    (* val newf : A.spec -> new *)
    (* type spec = A.spec *)
end

functor Bf (A: A) =
struct
structure A = A
datatype new = New of A.spec
end

signature C =
sig
    structure A : A
    datatype new = New of A.spec
    (* val newf : A.spec -> new *)
    (* type spec = A.spec *)
end

functor Cf (A: A) =
struct
structure A = A
datatype new = New of A.spec
end

signature D =
sig
    structure B : B
    structure C : C
    sharing B.A = C.A
end

structure BImp = Bf(AImp)
structure CImp = Cf(AImp)

functor Df () : D =
struct
structure B = BImp
structure C = CImp
fun convert x =
    case x of
        B.New x => C.New x
end
