type t = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type 'a textured = {
  texture : t;
  value : 'a
}

let textured t v = { texture = t; value = v }

(* let kd { texture = t; _ } = t.kd *)

(* let ks { texture = t; _ } = t.ks *)

(* let phong { texture = t; _ } = t.phong *)

(* let color { texture = t; _ } = t.color *)

let value { value = v; _ } = v

let texture { texture = t; _ } = t

let map f x = { x with value = f x.value }
