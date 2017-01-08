(* Colors *)
(**********)

type t

(* The value of each color component is between 0. and 1. *)
val make : float -> float -> float -> t

(* Same as make 0. 0. 0. *)
val black : t
val white : t

(* Convert to integer (r,g,b), each component in 0..255 *)
val to_bytes : t -> int*int*int

(* Convert to the color representation of the Graphics library *)
val to_graphics : t -> Graphics.color

(* Operations on color : addition / multiplication per component *)
val add : t -> t -> t
val mult : t -> t -> t
(* and multiplication of all compenents by a given float *)
val shift : float -> t -> t

(* Optional, for debugging purpose: *)
val print : t -> unit
