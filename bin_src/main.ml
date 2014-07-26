open Types
open Compiler

let initial = Const 0

let pair a b = Cons (Const a, Const b)

let turn = Fn (["cond"], (If (Var "cond",
                              pair 0 1,
                              pair 1 3)))

let step = Fn (["x"], Call (turn, [Var "x"]))

let e =  Cons (initial, step)

let instr = compile e

let () = instr |> assemble |> print_endline
