open Core_kernel.Std

open Types
open Printf

type code = Types.instruction list

let uid = let
  id = ref 0
  in fun x ->
    incr id; !id

let rec compile_expr expr fns =
  let bin_op e1 e2 op =
    let c1, f1 = compile_expr e1 fns in
    let c2, f2 = compile_expr e2 f1 in
    c1 @ c2 @ [op], f2
  in match expr with
    | Const x -> [LDC x], fns
    | Add (e1, e2)  -> bin_op e1 e2 ADD
    | Sub (e1, e2)  -> bin_op e1 e2 SUB
    | Mul (e1, e2)  -> bin_op e1 e2 MUL
    | Div (e1, e2)  -> bin_op e1 e2 DIV
    | Cons (e1, e2) -> bin_op e1 e2 CONS
    | Eq (e1, e2)   -> bin_op e1 e2 CEQ
    | Gt (e1, e2)   -> bin_op e1 e2 CGT
    | Gte (e1, e2)  -> bin_op e1 e2 CGTE
    | Arg i -> [LD (0, i)], fns
    | Fn e ->
      let id = uid() in
      let fns' = compile_func id e fns in
      [LDF id], fns'
    | If (cond, t, f) ->
      let (cond_code, f1) = compile_expr cond fns in
      let (true_code, f2) = compile_expr t f1 in
      let (false_code, f3) = compile_expr f f2 in
      let (true_addr, false_addr) = (uid(), uid()) in
      cond_code @ [SEL (true_addr, false_addr)],
      [LABEL true_addr :: true_code @ [JOIN]] @
      [LABEL false_addr :: false_code @ [JOIN]] @ f3

and compile_func id expr fns =
  let code, fns' = compile_expr expr fns in
  (LABEL id :: code @ [RTN]) :: fns'

let compile expr =
  let (code, fns) = compile_expr expr [] in
  code @ [RTN] @ List.concat fns

let rec assemble_rec instructions address_map =
  let resolve addr = Hashtbl.find_exn address_map addr in
  match instructions with
    | [] -> ""
    | (x :: xs) ->
      let cmd = match x with
        | LDC c -> sprintf "LDC %d" c
        | ADD -> "ADD"
        | SUB -> "SUB"
        | MUL -> "MUL"
        | DIV -> "DIV"
        | CONS -> "CONS"
        | RTN -> "RTN"
        | JOIN -> "JOIN"
        | LABEL _ -> ""
        | LD (i, j) -> sprintf "LD %d %d" i j
        | SEL (addr1, addr2) ->
          sprintf "SEL %d %d" (resolve addr1) (resolve addr2)
        | LDF addr -> sprintf "LDF %d" (resolve addr)
      in "  " ^ cmd ^ sprintf "\n%s" (assemble_rec xs address_map)

let assemble instructions =
  let address_map = Int.Table.create ~size:4 () in
  let (_ : int) = List.fold_left instructions
      ~init:0
      ~f:(fun acc -> function
          | LABEL l -> Hashtbl.add address_map l acc; acc
          | _       -> succ acc)
  in assemble_rec instructions address_map
