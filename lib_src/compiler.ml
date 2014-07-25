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
  let (code, fns) = compile_expr expr []
  in
  code @ [RTN] @ List.concat fns

let rec mk_addr_map instructions map addr =
  match instructions with
  | [] -> map
  | (x::xs) ->
    match x with
    | (LABEL l) -> Hashtbl.add map l addr; mk_addr_map xs map addr
    | _ -> mk_addr_map xs map (addr + 1)

let rec assemble_rec instructions address_map =
  let resolve_addr addr = Hashtbl.find address_map addr in
  match instructions with
  |[] -> ""
  |(x::xs) ->
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
      sprintf "SEL %d %d" (resolve_addr addr1) (resolve_addr addr2)
    | LDF address -> sprintf "LDF %d" (resolve_addr address)
    in "  " ^ cmd ^ sprintf "\n%s" (assemble_rec xs address_map)

let assemble instructions =
  let address_map = mk_addr_map instructions (Hashtbl.create 0) 0 in
  assemble_rec instructions address_map
