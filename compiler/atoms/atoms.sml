signature ATOMS =
sig
    structure Type : TYPE
    structure Operator : OPERATOR
    structure Const : CONST
    structure Id : ID
end

structure Atoms : ATOMS =
struct
structure Type = Type
structure Operator = Operator
structure Const = Const
structure Id = Id
end
