open Types
open Compiler

let e =  Cons (Const 0, Fn (Cons (Const 0, Const 1)))


let () = e |> assemble |> print_endline
