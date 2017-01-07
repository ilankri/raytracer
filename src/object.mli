type face = {
  normal_vect : Vect.t;         (** Outward normal vector.  *)
  center : Vect.t;              (** Center.  *)
  dist_orig : float;            (** Relative distance to origin.  *)
  opp_center : Vect.t;          (** Center of opposite face.  *)
  opp_dist_orig : float; (** Relative distance to origin of opposite face.  *)
  half_dist : float      (** Half of the distance to opposite face.  *)
}

type t =
  | Sphere of Vect.t * float    (** Center and radius.  *)

  | Plane of Vect.t * float
  (** Orthogonal vector to this plane with length 1 and distance from
      origin.  *)

  | Box of face Triple.t       (** Three faces with a common vertex.  *)

val xOz : t

val origin_box : Vect.t -> t
(** @return the origin-centered box with given diagonal vector.  *)

val translate : Vect.t -> t  -> t

val scale : float -> t  -> t

val rotate : Rotation.t -> t  -> t
