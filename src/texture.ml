type t = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type 'a textured = t * 'a

let textured t v = (t, v)

let texture (t, _) = t

let value (_, v) = v

let map f (t, v) = (t, f v)
