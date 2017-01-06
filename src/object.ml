type face = {
  norm_vect : Vect.t;
  center : Vect.t;
  dist : float;
  center_opp : Vect.t;
  dist_opp : float;
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
      let v_scal_n = Vect.scalprod v face.norm_vect in
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
      { face with norm_vect = apply_rot face.norm_vect;
                  center = apply_rot face.center;
                  center_opp = apply_rot face.center_opp }
    in
    transform_box (rotate_face rot) faces

let origin_box _ = failwith "TODO"
