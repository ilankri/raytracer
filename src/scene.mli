type light = {
  l_dir : Rotation.t;
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
  objects: Object.t list
}

val eval : Scenario.scenario -> int -> t
