type t = Color.t array array

let make width height scene =
  let img = Array.make_matrix height width Color.black in
  let cam_dist = Scene.(scene.camera.viewdist) in
  let ray_of_pixel x y =
    let x = float_of_int x in
    let y = float_of_int y in
    let half_width = float_of_int (width / 2) in
    let screen_point =
      Vect.make (x -. half_width) y (cam_dist -. half_width *. tan (1. /. 2.))
    in
    let src = Vect.make 0. 0. cam_dist in
    let dir = Vect.normalised_diff src screen_point in
    Beam.make src dir
  in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      img.(i).(j) <- Beam.trace (ray_of_pixel i j) 1 scene;
    done;
  done;
  img

let to_graphics img =
  let img_graphics =
    Array.(make_matrix (length img) (length img.(0))) Graphics.black
  in
  for i = 0 to Array.length img - 1 do
    for j = 0 to Array.length img.(0) - 1 do
      img_graphics.(i).(j) <- Color.to_graphics (img.(i).(j))
    done;
  done;
  Graphics.make_image img_graphics
