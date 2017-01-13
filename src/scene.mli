(** Scenario interpreter.  *)

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

val make : Scenario.scenario -> int -> t
