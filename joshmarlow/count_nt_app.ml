open Core.Std

let read_dna () =
    let dna_opt = In_channel.input_line stdin in
    match dna_opt with
        | None -> failwith "Please provide a DNA string\n"
        | Some dna -> dna

let read_dna_from_file filename =
    In_channel.read_all filename

let () =
    let dna = read_dna () in
    let a_count, c_count, g_count, t_count = Count_nt.count_nucleotides dna in
    Printf.printf "A: %d, C: %d, G: %d, T: %d\n" a_count c_count g_count t_count;
    ()
