type t = Matrix3.t

let id = Matrix3.id

type axis = Ox | Oy | Oz

(* Compute the rotation matrix through an angle alpha around the given
   axis.  *)
let rotation_matrix axis alpha =
  let cos_alpha = cos alpha in
  let sin_alpha = sin alpha in
  let l1, l2, l3 =
    match axis with
    | Ox ->
      Vect.xunit,
      Vect.make 0. cos_alpha (-.sin_alpha),
      Vect.make 0. sin_alpha cos_alpha
    | Oy ->
      Vect.make cos_alpha 0. sin_alpha,
      Vect.yunit,
      Vect.make (-.sin_alpha) 0. cos_alpha
    | Oz ->
      Vect.make cos_alpha (-.sin_alpha) 0.,
      Vect.make sin_alpha cos_alpha 0.,
      Vect.zunit
  in
  Matrix3.make l1 l2 l3

let compose r r' = Matrix3.mult r r'

let make rx ry rz =
  let mx = rotation_matrix Ox in
  let my = rotation_matrix Oy in
  let mz = rotation_matrix Oz in
  compose (mz rz) (compose (my ry) (mx rx))

let apply r v = Matrix3.mult_vect r v

let print r =
  print_string "rotation";
  Matrix3.print r
