let intersect bean obj =
match obj with
 | Plane(n, r) -> 
   (
    let p = Vect.scalprod n beam.source in
    if (p <= r) then None 
    else if (Vect.scalprod n beam.direction <=0 ) then None
    else
      let t = (r -. (Vect.scalprod n beam.source)) /. (Vect.scalprod n beam.direction) in 
        Vect.add beam.source (shift t beam.direction)
   )
 | _ -> failwith "Intersection of this object is not yet implemented"

(*type t = {
  source : Vect.t (* source of light from camera *)
  direction : Vect.t (* direction of light from camera *)
}
*)
