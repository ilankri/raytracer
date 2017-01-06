type face = {
  norm_vect : Vect.t;
  center : Vect.t;
  dist : float;
  center_opp : Vect.t;
  dist_opp : float;
  half_dist : float
}

type t =
  | Sphere of Vect.t * float (* center, radius *)
  | Plane of Vect.t * float (* vector orthogonal to this plan with length 1 and dist from origin *)
  | Box of face Triple.t

val xOz : t

val origin_box : Vect.t -> t

val translate : Vect.t -> t  -> t

val scale : float -> t  -> t

val rotate : Rotation.t -> t  -> t
