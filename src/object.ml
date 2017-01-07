type face = {
  normal_vect : Vect.t;
  center : Vect.t;
  dist_orig : float;
  opp_center : Vect.t;
  opp_dist_orig : float;
  half_dist : float
}

type t =
  | Sphere of Vect.t * float
  | Plane of Vect.t * float
  | Box of face Triple.t

let xOz = Plane (Vect.yunit, 0.)

let transform_box face_trans faces = Box (Triple.map face_trans faces)

let translate v = function
  | Sphere (c, r) -> Sphere (Vect.add v c, r)
  | Plane (n, d) -> Plane (n, d +. Vect.scalprod v n)
  | Box faces ->
    let translate_face v face =
      let add_v = Vect.add v in
      let v_scal_n = Vect.scalprod v face.normal_vect in
      { face with center = add_v face.center;
                  dist = face.dist +. v_scal_n ;
                  center_opp = add_v face.center_opp;
                  dist_opp = face.dist_opp -. v_scal_n }
    in
    transform_box (translate_face v) faces

let scale k = function
  | Sphere (c, r) -> Sphere (c, r *. k)
  | Plane (n, d) -> Plane (n, d *. k)
  | Box faces ->
    let scale_face k face =
      let shift_k = Vect.shift k in
      { face with center = shift_k face.center;
                  dist = k *. face.dist;
                  center_opp = shift_k face.center_opp;
                  dist_opp  = k *. face.dist_opp;
                  half_dist = k *. face.half_dist }
    in
    transform_box (scale_face k) faces

let rotate rot = function
  | Sphere (c, r) -> Sphere (Rotation.apply rot c, r)
  | Plane (n, d) -> Plane (Rotation.apply rot n, d)
  | Box faces ->
    let rotate_face rot face =
      let apply_rot = Rotation.apply rot in
      { face with normal_vect = apply_rot face.normal_vect;
                  center = apply_rot face.center;
                  center_opp = apply_rot face.center_opp }
    in
    transform_box (rotate_face rot) faces

let origin_box diag_vect =
  let compute_face normal_vect dist_to_opp_face =
    let dist = 0.5 *. dist_to_opp_face in
    let center = Vect.shift dist normal_vect in
    {
      normal_vect = normal_vect;
      center = center;
      dist = dist;
      center_opp = Vect.opp center;
      dist_opp = -. dist;
      half_dist = dist;
    }
  in
  let std_basis = Triple.make Vect.xunit Vect.yunit Vect.zunit in
  let dims =
    Triple.make (Vect.vx diag_vect) (Vect.vy diag_vect) (Vect.vz diag_vect)
  in
  Box (Triple.map2 compute_face std_basis dims)
