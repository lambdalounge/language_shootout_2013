open Core.Std

let count_nucleotides dna =
    String.fold dna ~init:(0, 0, 0, 0) ~f:begin fun (a_acc, c_acc, g_acc, t_acc) c ->
        match c with
            | 'A' -> (succ a_acc, c_acc, g_acc, t_acc)
            | 'C' -> (a_acc, succ c_acc, g_acc, t_acc)
            | 'G' -> (a_acc, c_acc, succ g_acc, t_acc)
            | 'T' -> (a_acc, c_acc, g_acc, succ t_acc)
            | _dontcare -> failwith (Printf.sprintf "Unexpected character %c" c);
    end
