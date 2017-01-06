type face = {
  normal_vect : Vect.t;
  center : Vect.t;
  dist : float;
  center_opp : Vect.t;
  dist_opp : float;
  half_dist : float
}

type t =
  | Sphere of Vect.t * float (* center, radius *)
  | Plane of Vect.t * float (* normal vector and dist from origin *)
  | Box of face Triple.t

val xOz : t

val origin_box : Vect.t -> t
(** @return the origin-centered box with given diagonal vector.  *)

val translate : Vect.t -> t  -> t

val scale : float -> t  -> t

val rotate : Rotation.t -> t  -> t
