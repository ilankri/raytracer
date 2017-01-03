(* A vector is represented by its coordinates (x, y, z) in the
   space.  *)
type t = float Triple.t

let vx v = Triple.fst v

let vy v = Triple.snd v

let vz v = Triple.trd v

let make x y z = Triple.make x y z

let xunit = make 1. 0. 0.

let yunit = make 0. 1. 0.

let zunit = make 0. 0. 1.

let scalprod v v' = vx v *. vx v' +. vy v *. vy v' +. vz v *. vz v'

let norm v = sqrt (scalprod v v)

let add v v' = Triple.map2 (+.) v v'

let diff v v' = Triple.map2 (-.) v v'

let opp v = Triple.map (~-.) v

let shift k v = Triple.map (( *. ) k) v

let dist p p' = norm (diff p p')

(* BEWARE: normalise can return (infinity, infinity, infinity).  *)
let normalise v = shift (1. /. (norm v)) v

let dist2 p p' =
  let diff_v = diff p p' in
  scalprod diff_v diff_v

let normalised_diff v v' = normalise (diff v v')

let print v = Printf.printf "(%f, %f, %f)" (vx v) (vy v) (vz v)
