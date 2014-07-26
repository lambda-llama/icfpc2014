open Types
open Compiler
open Stdlib

let initial = Const 0

let step = scope
             "turn" (Fn (["cond"], (If (Var "cond",
                                        pair 0 1, pair 1 3))))
             (Fn (["x"], Call (Var "turn", [Var "x"])))

let e =  Cons (initial, step)

let instr = compile e

let () = instr |> assemble |> print_endline
