signature APPRLISTMAP =
sig
    structure LA: APPRLIST
    structure LB: APPRLIST
    val map: (LA.A.t -> LB.A.t) -> LA.A.t LA.t -> LB.A.t LB.t
end

functor ApprListMap (structure LA: APPRLIST
                     structure LB: APPRLIST) =
struct
structure LA = LA
structure LB = LB
fun map f l =
    let
        val len = LA.length l
        val samples = LA.samples l
        val samples =
            List.map (fn (idx, r) => (idx, f r)) samples
    in
        LB.build samples len
    end
fun mapi f l =
    let
        val len = LA.length l
        val samples = LA.samples l
        val samples =
            List.map (fn (idx, r) => (idx, f (idx, r))) samples
    in
        LB.build samples len
    end
end
