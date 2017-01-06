val intersect : Beam -> Object.t -> Vect.t option

type Beam = {
  source : Vect.t (* source of light from camera *)
  direction : Vect.t (* direction of light from camera *)
}
