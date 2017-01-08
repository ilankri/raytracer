type t = float Triple.t

let make r g b = Triple.make r g b

let black = make 0. 0. 0.

(* This function leads to a precision loss because of the use of
   int_of_float (can be improved by computing the nearest integer
   instead).  *)
let to_bytes c =
  let ceiling component = if component > 1. then 1. else component in
  Triple.(to_tuple (map (fun x -> int_of_float (255. *. ceiling x)) c))

let to_graphics c =
  let r, g, b = to_bytes c in
  Graphics.rgb r g b

let add c c' = Triple.map2 (+.) c c'

let mult c c' = Triple.map2 ( *. ) c c'

let shift k c = Triple.map (( *. ) k) c

let print c =
  let r, g, b = to_bytes c in
  Printf.printf "color(%d, %d, %d)" r g b
