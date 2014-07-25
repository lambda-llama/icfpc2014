open Types
open Compiler

let initial = Const 0

let step = Fn (If (Arg 0,
                  Cons (Const 0, Const 1),
                  Cons (Const 1, Const 3)))

let e =  Cons (initial, step)

let instr = compile e

let () = instr |> assemble |> print_endline
