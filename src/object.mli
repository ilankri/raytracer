(** Geometric objects.  *)

type face = private {
  normal_vect : Vect.t;         (** Outward normal vector.  *)
  center : Vect.t;              (** Center.  *)
  dist_orig : float;            (** Relative distance to origin.  *)
  opp_center : Vect.t;          (** Center of opposite face.  *)
  opp_dist_orig : float; (** Relative distance to origin of opposite face.  *)
  half_dist : float      (** Half of the distance to opposite face.  *)
}

type t = private
  | Sphere of Vect.t * float    (** Center and radius.  *)

  | Plane of Vect.t * float
  (** Orthogonal vector to this plane with length 1 and distance from
      origin.  *)

  | Box of face Triple.t       (** Three faces with a common vertex.  *)

val origin_box : Vect.t -> t
(** @return the origin-centered box with given diagonal vector.  *)

val translate : Vect.t -> t  -> t

val scale : float -> t  -> t

val rotate : Rotation.t -> t  -> t

val sphere : Vect.t -> float -> t
(** @return a sphere with the given center and radius.  *)

val plane : Vect.t -> float -> t
(** @return a plane with given outward normal vector and distance to
    origin.  *)
