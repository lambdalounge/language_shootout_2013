open OUnit2

let test__count_nucleotides _ =
    let dna = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" in
    let expected_counts = (20, 12, 17, 21) in
    assert_equal expected_counts (Count_nt.count_nucleotides dna);
    ()

let all_tests =
    "all_tests">:::[
        "test__count_nucleotides">::test__count_nucleotides;
    ];;

let () =
    run_test_tt_main all_tests;
    ()
