signature ATOMS =
sig
    structure Operator : OPERATOR
    structure Const : CONST
    structure Id : ID
end

structure Atoms : ATOMS =
struct
structure Operator = Operator
structure Const = Const
structure Id = Id
end
