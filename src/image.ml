type t = Color.t array array

let make width height scene =
  let img = Array.make_matrix height width Color.black in
  let ray_of_pixel x y =
    let x = float_of_int x in
    let y = float_of_int y in
    let screen_point =
      Vect.make
        (x -. float_of_int (width asr 1))
        (float_of_int (height asr 1) -. y)
        Scene.(scene.camera.viewdist -.
               float_of_int width /.
               ldexp (tan (ldexp scene.camera.angle (-1))) 1)
    in
    let src = Vect.make 0. 0. Scene.(scene.camera.viewdist) in
    let dir = Vect.normalised_diff screen_point src in
    Beam.make src dir
  in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      img.(i).(j) <- Beam.trace (ray_of_pixel j i) 10 scene;
    done;
  done;
  img

let to_graphics img =
  let img_graphics =
    Array.(make_matrix (length img) (length img.(0))) Graphics.black
  in
  for i = 0 to Array.length img - 1 do
    for j = 0 to Array.length img.(0) - 1 do
      img_graphics.(i).(j) <- Color.to_graphics img.(i).(j)
    done;
  done;
  Graphics.make_image img_graphics
