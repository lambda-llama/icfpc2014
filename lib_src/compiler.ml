open Core_kernel.Std

open Types
open Printf

type code = Types.instruction list

let uid = let
  id = ref 0
  in fun x ->
    incr id; !id

type env = {
  vars   : var list;
  parent : env option;
}

let push_vars env vars = {
  vars = vars;
  parent = Some env;
}

let rec index xs x = match xs with
  | [] -> None
  | (y::ys) -> if x = y
               then Some 0
               else Option.map (index ys x) succ

let rec lookup {vars; parent} v =
  match index vars v with
  | Some i -> (0, i)
  | None ->
     match parent with
     | None -> failwith "Ouups, udefined variable"
     | Some env ->
        let (e, i) = lookup env v in
        (succ e, i)

type state = {
  functions : code list;
  env : env;
}

let initial_state = {
  functions = [];
  env = {vars = []; parent = None}
}

let add_fn label code ret {functions; env} =
  let wrapped_code = LABEL label :: code @ [ret] in
  {functions = wrapped_code::functions;
   env = env}

let rec compile_expr expr state =
  let bin_op e1 e2 op =
    let c1, s1 = compile_expr e1 state in
    let c2, s2 = compile_expr e2 s1 in
    c1 @ c2 @ [op], s2
  in match expr with
    | Const x -> [LDC x], state
    | Add (e1, e2)  -> bin_op e1 e2 ADD
    | Sub (e1, e2)  -> bin_op e1 e2 SUB
    | Mul (e1, e2)  -> bin_op e1 e2 MUL
    | Div (e1, e2)  -> bin_op e1 e2 DIV
    | Cons (e1, e2) -> bin_op e1 e2 CONS
    | Eq (e1, e2)   -> bin_op e1 e2 CEQ
    | Gt (e1, e2)   -> bin_op e1 e2 CGT
    | Gte (e1, e2)  -> bin_op e1 e2 CGTE
    | Fn (formals, e)->
      let id = uid() in
      let s1 = compile_func id formals e state in
      [LDF id], s1
    | Var var ->
       let (e, i) = lookup state.env var in
       [LD (e, i)], state
    | If (cond, t, f) ->
      let (cond_code, s1) = compile_expr cond state in
      let (true_code, s2) = compile_expr t s1 in
      let (false_code, s3) = compile_expr f s2 in
      let (true_addr, false_addr) = (uid(), uid()) in
      (cond_code @ [SEL (true_addr, false_addr)],
       add_fn true_addr true_code JOIN s3 |>
         add_fn false_addr false_code JOIN)

and compile_func id formals expr {functions; env} =
  let e_env = push_vars env formals in
  let e_state = {functions = functions; env = e_env} in
  let code, {functions = fns1} = compile_expr expr e_state in
  add_fn id code RTN {functions = fns1; env=env}

let compile expr =
  let (code, {functions}) = compile_expr expr initial_state in
  code @ [RTN] @ List.concat functions

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
