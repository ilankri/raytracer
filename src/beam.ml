type t = {
  source : Vect.t; (* source of light from camera *)
  direction : Vect.t (* direction of light from camera *)
}

let make src dir = { source = src; direction = dir }

let intersect beam = Object.(function
    | Plane (n, r) ->
      let p = Vect.scalprod n beam.source in
      if p <= r then None
      else if Vect.scalprod n beam.direction <= 0. then None
      else
        let t =
          (r -. (Vect.scalprod n beam.source))
          /. (Vect.scalprod n beam.direction)
        in
        Some (Vect.add beam.source (Vect.shift t beam.direction))
    | _ -> failwith "Intersection of this object is not yet implemented"
  )

(* TODO *)
let trace ray nb_max_reb scene = Color.make 0. 0. 1.
