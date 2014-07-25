open Types
open Printf


let rec compile = function
| Const x -> [LDC x]
| Add (e1, e2) -> compile e1 @ compile e2 @ [ADD]
;;

let rec assemble instructions =
  match instructions with
  |[] -> ""
  |(x::xs) ->
    let cmd = match x with
    | LDC x -> sprintf "LDC %d" x
    | ADD -> sprintf "ADD"
    in "  " ^ cmd ^ sprintf "\n%s" (assemble xs)
