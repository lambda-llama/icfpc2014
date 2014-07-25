open Types
open Compiler

let e = Add (Const 92, Const 1);;

let instr = compile e

let () = instr |> assemble |> print_endline
