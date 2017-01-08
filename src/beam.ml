type t = {
  src : Vect.t; (* source of light from camera *)
  dir : Vect.t (* direction of light from camera *)
}

let make s d = { src = s; dir = d }

type impact = {
  point : Vect.t;
  norm_vect : Vect.t;
}

let position ray t = Vect.(ray.dir |> shift t |> add ray.src)

let intersect beam = Object.(function
    | Plane (n, d) ->
      let p = Vect.scalprod n beam.src in
      if p <= d then None
      else if Vect.scalprod n beam.dir >= 0. then None
      else
        let t =
          (d -. (Vect.scalprod n beam.src))
          /. (Vect.scalprod n beam.dir)
        in
        Some { point = position beam t; norm_vect = n }
    | Sphere (c, r) ->
      let sc_v = Vect.diff c beam.src in
      let sa = Vect.scalprod sc_v beam.dir in
      if sa <= 0. then None else
        let sc2 = Vect.dist2 c beam.src in
        let ac2 = sc2 -. sa *. sa in
        if sqrt ac2 >= r then None else
          let ab = sqrt (r *. r -. ac2) in
          let sb = sa -. ab in
          let b = position beam sb in
          Some { point = b; norm_vect = Vect.(c |> diff b |> shift (1. /. r)) }
    | _ -> failwith "Intersection of this object is not yet implemented"
  )

let first_impact ray objs =
  let rec aux ray dist surface = function
    | [] -> surface
    | obj :: objs ->
      begin match intersect ray (Texture.untextured obj) with
        | Some ({ point = p; norm_vect = n } as impact) ->
          let dist' = Vect.dist p ray.src in
          if dist' < dist then
            aux ray dist' (Some (Texture.(textured (texture obj) impact))) objs
          else
            aux ray dist surface objs
        | _ -> aux ray dist surface objs
      end
  in
  aux ray infinity None objs

let visible_light norm_v impact_pt objs light =
  Vect.scalprod light.Scene.l_dir norm_v < 0. &&
  first_impact { src = impact_pt; dir = Vect.opp light.Scene.l_dir } objs = None

let weighted_sum compute_term lights =
  List.fold_left (fun acc l -> acc +. compute_term l *. l.Scene.l_intensity)
    0. lights

let rec trace ray depth scene =
  match first_impact ray scene.Scene.objects with
  | None -> Color.black
  | Some impact ->
    let { point = p; norm_vect = n }, t = Texture.destruct impact in
    let compute_term light = Vect.scalprod n light.Scene.l_dir in
    let compute_term' light =
      let hj = Vect.normalised_diff (Vect.opp light.Scene.l_dir) ray.dir in
      Vect.scalprod n hj ** t.Texture.phong
    in
    let visible_lights = List.filter (visible_light n p scene.Scene.objects)
        scene.Scene.lights in
    let k =
      t.Texture.kd *. (scene.Scene.ambient -.
                       weighted_sum compute_term visible_lights)
    in
    let k' = t.Texture.ks *. weighted_sum compute_term' visible_lights in
    let c = Color.shift (k +. k') t.Texture.color in
    if depth = 0 then c else
      let dir_refl_ray =
      Vect.normalised_diff
        ray.dir
        Vect.(shift (ldexp (scalprod n ray.dir) 1) n)
      in
      let refl_ray = { src = p; dir = dir_refl_ray } in
      Color.(add c (shift t.Texture.ks
                      (trace refl_ray (depth - 1) scene)))
