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

let translate vect = function
  | Sphere (center, radius, texture) ->
    Sphere (Vect.add center vect, radius, texture)
  | Plane (rotation, dist, texture) -> failwith "todo"
  | Box (center, diag_vect, texture) -> failwith "todo"

let scale coeff = function
  | Sphere (center, radius, texture) ->
    Sphere (center, radius *. coeff, texture)
  | Plane (rotation, dist, texture) -> failwith "todo"
  | Box (center, diag_vect, texture) -> failwith "todo"

let rotate rot = function
  | Sphere (center, radius, texture) -> failwith "todo"
  | Plane (rotation, dist, texture) -> failwith "todo"
  | Box (center, diag_vect, texture) -> failwith "todo"
