(** Ray tracing.  *)

type t

val make : Vect.t -> Vect.t -> t
(** @return the ray with given source and direction.  *)

val trace : t -> int -> Scene.t -> Color.t
(** [trace ray nb_max_reb scene] computes the color "viewed" by
    [ray] in [scene] with at most [nb_max_reb] rebounds.  *)
