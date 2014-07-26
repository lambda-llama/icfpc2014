open Sexplib

open Gcc_types

let () =
  let sexp = Sexp.input_sexp stdin in
  let expr = expr_of_sexp sexp in
  let open Gcc_compiler in
  compile expr |> assemble |> print_endline
