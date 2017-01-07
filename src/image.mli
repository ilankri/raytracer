type t

val make : int -> int-> Scene.t -> t
(** [make width height scene] computes the image of size [width*height]
    corresponding to [scene].  *)

val to_graphics : t -> Graphics.image
