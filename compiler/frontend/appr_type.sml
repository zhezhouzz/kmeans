signature APPR_TYPE =
sig
    structure Type : TYPE
    datatype t =
             AMap of Type.t * Type.t
             | AFoldl of Type.t
             | AMapi of Type.t * Type.t
             | AFoldli of Type.t
             | ANth of Type.t
             | ASample of Type.t
end

functor ApprType (Type: TYPE) : APPR_TYPE =
struct
structure Type = Type
datatype t =
         AMap of Type.t * Type.t
         | AFoldl of Type.t
         | AMapi of Type.t * Type.t
         | AFoldli of Type.t
         | ANth of Type.t
         | ASample of Type.t
end
