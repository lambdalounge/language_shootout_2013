open Core.Std

let compute_homozygous_recessive k m n =
    let k' = Float.of_int k in
    let m' = Float.of_int m in
    let n' = Float.of_int n in
    let p = k' +. m' +. n' in
    (** Probability given two n parents ***)
    let p_nn = n'/.p *. (n' -. 1.0)/.(p -. 1.0) in
    let p_mm = m'/.p *. (m' -. 1.0)/.(p -. 1.0) in
    let p_nm = m'/.p *. n'/.(p -. 1.0) in
    1.0 -. (p_nn +. 0.25 *. p_mm +. p_nm)