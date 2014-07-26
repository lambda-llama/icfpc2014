let () =
  let open OUnit2 in
  ignore (run_test_tt_main (Test.all ()))
