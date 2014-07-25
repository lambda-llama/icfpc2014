type code = Types.instruction list

val compile : Types.expr -> code

val assemble : code -> string
