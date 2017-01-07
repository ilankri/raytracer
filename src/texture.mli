type t = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type 'a textured

val textured : t -> 'a -> 'a textured

val texture : 'a textured -> t

val value : 'a textured -> 'a

val map : ('a -> 'b) -> 'a textured -> 'b textured
