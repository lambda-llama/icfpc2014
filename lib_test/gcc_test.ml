open Core_kernel.Std

open OUnit2

open Gcc_types


let scope name var expr = Call (Fn ([name], expr), [var])


let test_gcc ~ast ~path =
  let open Gcc_compiler in
  let asm      = compile ast |> assemble
  and expected = In_channel.read_all path |> String.rstrip
  in assert_equal ~printer:(sprintf "%S") expected asm


let test_local test_ctx =
  test_gcc
    ~ast:(scope "x" (Const 21) (Add ((Var "x"), (Var "x"))))
    ~path:"lib_test/local.gcc"


let test = "GCC" >::: [
    "local.gcc" >:: test_local;
  ]
