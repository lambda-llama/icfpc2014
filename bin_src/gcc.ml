open Sexplib
open Sexplib.Std

open Gcc_types


let () =
  try
    let sexp = Sexp.input_sexp stdin in
    let expr = expr_of_sexp sexp in
    let open Gcc_compiler in
    compile expr |> assemble |> print_endline
  with e -> sexp_of_exn e |> Sexp.to_string |> print_endline
