type t = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type 'a textured

val textured : t -> 'a -> 'a textured

val value : 'a textured -> 'a

val texture : 'a textured -> t

val map : ('a -> 'b) -> 'a textured -> 'b textured