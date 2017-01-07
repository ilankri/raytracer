type t

val make : Vect.t -> Vect.t -> t

val intersect : t -> Object.t -> Vect.t option

val trace : t -> int -> Scene.t -> Color.t
(** [trace ray nb_max_reb scene] computes the color "viewed" by
    [ray] in [scene] with at most [nb_max_reb] rebounds.  *)
