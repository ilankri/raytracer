(* Abstract Syntax of Instructions and Scenario *)
(************************************************)

(* Binary Operators *)
type binop = Plus | Minus | Mult | Div

(* Unary Operators *)
type unop = Sin | Cos | Sqrt | Opp

(* Numerical Expressions *)
type expr =
  | Bin of binop * expr * expr
  | Uni of unop * expr
  | Const of float
  | Ident of string

type color = expr * expr * expr (* rgb components, in 0..255 *)
type rotation = expr * expr * expr (* rotation angles around x, y, z axis *)
type position = expr * expr * expr
type vector = expr * expr * expr

type texture = {
  kd : expr;
  ks : expr;
  phong : expr;
  color : color }

type obj =
  | Object of string (* object identifier *)
  | Sphere of position * expr * texture (* center, radius *)
  | Plane of rotation * expr * texture
  (* horizontal plane (xOz, normal vector Oy),
     then rotated and put at some dist from origin *)
  | Box of position * vector * texture
  (* center, diagonal vector a.k.a. width/height/depth *)
  | Translate of obj * vector (* object, translation vector *)
  | Scale of obj * expr (* object, scale factor *)
  | Rotate of obj * rotation (* object, rotation *)
  | Group of obj list

type boolean =
  | And of boolean * boolean
  | Or of boolean * boolean
  | Not of boolean
  | Equal of expr * expr
  | Less of expr * expr

type instruction =
  | SetNum of string * expr
  | SetObj of string * obj
  | Call of string * (expr list)
  | Put of obj
  | If of boolean * instruction list * instruction list

type proc = { name : string; params : string list; body : instruction list }

(* Light.
   By default it comes from above (along y axis, toward the xOz plane).
   Then we apply to it the given rotation. *)
type light = { l_dir : rotation; l_intensity : expr }

type camera = { viewdist : expr; angle : expr }

type scenario = {
  camera : camera;
  ambient : expr;
  lights : light list;
  procs : proc list;
  main : instruction list
}
