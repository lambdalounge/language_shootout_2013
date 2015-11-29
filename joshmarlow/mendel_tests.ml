open Core.Std
open OUnit2

let test__compute_homozygous_recessive _ =
    let k, m, n = 2, 2, 2 in
    let expected_p = 0.78333 in
    let epsilon = 0.1 in
    let p = Mendel.compute_homozygous_recessive k m n in
    assert_bool (Printf.sprintf "Expected: %f, received: %f\n" expected_p p)
                (epsilon > (Float.abs (expected_p -. p)));
    ()

let all_tests =
    "all_tests">:::[
        "test__compute_homozygous_recessive">::test__compute_homozygous_recessive;
    ];;

let () =
    run_test_tt_main all_tests;
    ()
