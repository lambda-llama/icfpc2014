open Core_kernel.Std

open OUnit2

open Types
open Compiler
open Stdlib


let test_gcc ~ast ~path =
  let asm      = compile ast |> assemble
  and expected = In_channel.read_all path
  in assert_equal ~printer:(sprintf "%S") expected asm


let test_local test_ctx =
  test_gcc
    ~ast:(scope "x" (Const 21) (Add ((Var "x"), (Var "x"))))
    ~path:"lib_test/local.gcc"


let test_goto test_ctx =
  ()


let test = "GCC" >::: [
    "local.gcc" >:: test_local
    "goto.gcc"  >:: test_goto
  ]
