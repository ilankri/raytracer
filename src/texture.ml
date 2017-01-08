type t = {
  kd : float;
  ks : float;
  phong : float;
  color : Color.t
}

type 'a textured = t * 'a

let textured t x = (t, x)

let destruct (t, x) = (x, t)

let texture (t, _) = t

let untextured (_, x) = x

let map f (t, x) = (t, f x)
