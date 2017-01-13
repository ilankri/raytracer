(** Image calculation.  *)

type t

val make : int -> int-> int -> Scene.t -> t
(** [make width height depth scene] computes the image of size
    [width*height] corresponding to [scene] with at most [depth] bounces
    for the rays.  *)

val to_graphics : t -> Graphics.image
