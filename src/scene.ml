(* TODO: handle exceptions -> List.find, Dict.lookup,
   List.fold_left2.  *)

(* Types to represent a scene.  *)
type light = {
  l_dir : Vect.t;
  l_intensity : float
}

type camera = {
  viewdist : float;
  angle : float
}

type t = {
  camera : camera;
  ambient : float;
  lights : light list;
  objects: Object.t Texture.textured list
}

(* Evaluation environment.  *)
type environment = {
  nums : (string, float) Dict.t;
  objs : (string, Object.t Texture.textured list) Dict.t;
  procs : Scenario.proc list;
}

let add_num env id num = { env with nums = Dict.insert id num env.nums }

let add_obj env id obj = { env with objs = Dict.insert id obj env.objs }

let lookup_num env id = Dict.lookup id env.nums

let lookup_obj env id = Dict.lookup id env.objs

let lookup_proc env id =
  List.find (fun proc -> proc.Scenario.name = id) env.procs

(* Evaluation.  *)
let eval_binop = Scenario.(function
    | Plus -> (+.)
    | Minus -> (-.)
    | Mult -> ( *. )
    | Div -> (/.)
  )

let eval_unop = Scenario.(function
    | Sin -> sin
    | Cos -> cos
    | Sqrt -> sqrt
    | Opp -> (~-.)
  )

let rec eval_expr env = Scenario.(function
    | Bin (binop, e1, e2) ->
      (eval_binop binop) (eval_expr env e1) (eval_expr env e2)
    | Uni (unop, e) -> (eval_unop unop) (eval_expr env e)
    | Const c -> c
    | Ident id -> lookup_num env id
  )

let eval_expr3 make env (e1, e2, e3) =
  make (eval_expr env e1) (eval_expr env e2) (eval_expr env e3)

let eval_color env c =
  eval_expr3 (fun r g b -> Color.make (r /. 255.) (g /. 255.) (b /. 255.)) env c

let eval_rotation env r = eval_expr3 Rotation.make env r

let eval_vector env v = eval_expr3 Vect.make env v

let eval_camera env camera = {
  viewdist = eval_expr env camera.Scenario.viewdist;
  angle = eval_expr env camera.Scenario.angle
}

let eval_texture env texture = {
  Texture.kd = eval_expr env texture.Scenario.kd;
  Texture.ks = eval_expr env texture.Scenario.ks;
  Texture.phong = eval_expr env texture.Scenario.phong;
  Texture.color = eval_color env texture.Scenario.color
}

let eval_obj env obj =
  let eval_expr' = eval_expr env in
  let eval_vector' = eval_vector env in
  let eval_rotation' = eval_rotation env in
  let textured texture obj =
    [Texture.textured (eval_texture env texture) obj]
  in
  let rec aux = Scenario.(function
      | Object id -> lookup_obj env id
      | Sphere (center, radius, texture) ->
        textured texture
          (Object.Sphere (eval_vector' center, eval_expr' radius))
      | Plane (rotation, dist, texture) ->
        textured texture Object.(
            xOz
            |> rotate (eval_rotation' rotation)
            |> translate (Vect.shift (eval_expr' dist) Vect.yunit)
          )
      | Box (center, diag_vect, texture) ->
        textured texture Object.(
            translate
              (eval_vector' center)
              (diag_vect |> eval_vector' |> origin_box)
          )
      | Translate (obj, v) ->
        map (v |> eval_vector' |> Object.translate) obj
      | Scale (obj, k) ->
        map (k |> eval_expr' |> Object.scale) obj
      | Rotate (obj, rot) ->
        map (rot |> eval_rotation' |> Object.rotate) obj
      | Group [] -> []
      | Group (obj :: objl) -> aux obj @ aux (Scenario.Group objl)
    )
  and map trans obj = List.map (Texture.map trans) (aux obj) in
  aux obj

let rec eval_boolean env = Scenario.(function
    | And (b1, b2) -> eval_boolean env b1 && eval_boolean env b2
    | Or (b1, b2) -> eval_boolean env b1 || eval_boolean env b2
    | Not (b) -> not (eval_boolean env b)
    | Equal (e1, e2) -> eval_expr env e1 = eval_expr env e2
    | Less (e1, e2) -> eval_expr env e1 < eval_expr env e2
  )

let rec eval_instruction env objs = Scenario.(function
    | SetNum (id, e) -> (add_num env id (eval_expr env e), objs)
    | SetObj (id, o) -> (add_obj env id (eval_obj env o), objs)
    | Put o -> (env, (eval_obj env o) @ objs)
    | If (b, il1, il2) ->
      eval_instr_list (env, objs) (if eval_boolean env b then il1 else il2)
    | Call (id, args) ->
      let proc = lookup_proc env id in
      let eval_and_bind_arg env param arg =
        add_num env param (eval_expr env arg)
      in
      let local_env = List.fold_left2 eval_and_bind_arg env proc.params args in
      let _, objs = eval_instr_list (local_env, objs) proc.body in
      (env, objs)
  )

and eval_instr_list (env, objs) il =
  List.fold_left (fun (env, objs) -> eval_instruction env objs) (env, objs) il

let eval_light env light = {
  l_dir =
    Rotation.apply
      (eval_rotation env light.Scenario.l_dir)
      (Vect.opp Vect.yunit);
  l_intensity = eval_expr env light.Scenario.l_intensity
}

let make scenario time =
  let env = {
    nums = Dict.empty;
    objs = Dict.empty;
    procs = scenario.Scenario.procs
  }
  in
  let env = add_num env "time" (float_of_int time) in
  {
    camera = eval_camera env scenario.Scenario.camera;
    ambient = eval_expr env scenario.Scenario.ambient;
    lights = List.map (eval_light env) scenario.Scenario.lights;
    objects = snd (eval_instr_list (env, []) scenario.Scenario.main)
  }
