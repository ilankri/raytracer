(* Rotations dans l'espace tri-dimensionnel. *)

(* Une rotation est créée à partir de trois nombres [rx], [ry] et [rz]
    qui représentent :
   - une rotation d'angle [rx] autour de l'axe des [x]
   - puis une rotation d'angle [ry] autour de l'axe des [y]
   - puis une rotation d'angle [rz] autour de l'axe des [z]

    Ces trois rotations élémentaires se font chacunes dans le sens
    inverse des aiguilles d'une montre. On peut utiliser la "règle
    de la main droite" pour orienter les axes et ces rotations
    élémentaires. Les angles sont exprimés en radians.
*)

type t

(* Construction d'une rotation.
   Les trois arguments sont [rx], [ry] et [rz] dans cet ordre. *)

val make : float -> float -> float -> t

(* Identité. *)

val id : t

(*s Composition de deux rotations. [(compose r1 r2)] représente [r1]
    $\circ$ [r2] i.e. [r2] puis [r1]. *)

val compose : t -> t -> t

(*s [(applique r v)] applique la rotation [r] au vecteur [v]. *)

val apply : t -> Vect.t -> Vect.t

(* Eventuellement, pour du debug: *)
val print : t -> unit
