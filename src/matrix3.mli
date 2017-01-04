(** 3-by-3 real matrices.  *)

type t
(** Abstract type of 3-by-3 real matrices.  *)

val id : t
(** The identity matrix.  *)

val make : Vect.t -> Vect.t -> Vect.t -> t
(** @return the 3-by-3 matrix whose lines are the given vectors in the
    same order.  *)

val mult : t -> t -> t
(** Matrix multiplication.  *)

val mult_vect : t -> Vect.t -> Vect.t
(** [mult_vect m v] returns the product of [m] multiplied by the column
    vector [v].  *)

val print : t -> unit
