type texture = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type t =
  | Sphere of Vect.t * float * texture (* center, radius *)
  | Plane of Rotation.t * float * texture
  (* horizontal plane (xOz, normal vector Oy),
     then rotated and put at some dist from origin *)
  | Box of Vect.t * Vect.t * texture
  (* center, diagonal vector a.k.a. width/height/depth *)

val translate : Vect.t -> t  -> t

val scale : float -> t  -> t

val rotate : Rotation.t -> t  -> t
