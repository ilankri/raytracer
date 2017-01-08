type t

val make : Vect.t -> Vect.t -> t

type impact

val intersect : t -> Object.t -> impact option
(** [intersect ray obj] returns, if [ray] intersects [obj], the impact
    point defined by the intersection point and the unit normal vector
    outward from impact surface.  *)

val trace : t -> int -> Scene.t -> Color.t
(** [trace ray nb_max_reb scene] computes the color "viewed" by
    [ray] in [scene] with at most [nb_max_reb] rebounds.  *)
