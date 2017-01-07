type t = float Triple.t

let make r g b = Triple.make r g b

let black = make 0. 0. 0.

(* This function leads to a precision loss because of the use of
   int_of_float (can be improved by computing the nearest integer
   instead).  *)
let to_bytes c =
  Triple.to_tuple (Triple.map (fun x -> int_of_float (x *. 255.)) c)

let to_graphics c =
  let r, g, b = to_bytes c in
  Graphics.rgb r g b

let add c c' = Triple.map2 (+.) c c'

let mult c c' = Triple.map2 ( *. ) c c'

let shift k c = Triple.map (( *. ) k) c

let print c =
  Printf.printf "color(%f, %f, %f)" (Triple.fst c) (Triple.snd c) (Triple.trd c)
