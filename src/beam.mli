type t

val make : Vect.t -> Vect.t -> t

val intersect : t -> Object.t -> (Vect.t * Vect.t) option
(** [intersect ray obj] returns, if [ray] intersects [obj], the pair
    [(p, n)] with [p] the intersection point and [n] the unit normal
    vector outward from impact surface.  *)

val trace : t -> int -> Scene.t -> Color.t
(** [trace ray nb_max_reb scene] computes the color "viewed" by
    [ray] in [scene] with at most [nb_max_reb] rebounds.  *)
