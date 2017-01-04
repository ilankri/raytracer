type t = float array array

let new_zero_matrix n p () = Array.make_matrix n p 0.

let make v1 v2 v3 =
  let array_of_vect v = [|Vect.vx v; Vect.vy v; Vect.vz v|] in
  let l1 = array_of_vect v1 in
  let l2 = array_of_vect v2 in
  let l3 = array_of_vect v3 in
  let m = new_zero_matrix 3 3 () in
  m.(0) <- l1;
  m.(1) <- l2;
  m.(2) <- l3;
  m

(* Naive algorithm is sufficient because we work with small
   matrices.  *)
let mult m m' =
  let n = Array.length m in
  let q = Array.length m' in
  let p = Array.length m'.(0) in
  let res = new_zero_matrix n p () in
  for i = 0 to n - 1 do
    for j = 0 to p - 1 do
      for k = 0 to q - 1 do
        res.(i).(j) <- res.(i).(j) +. m.(i).(k) *. m'.(k).(j)
      done
    done
  done;
  res

let id = make Vect.xunit Vect.yunit Vect.zunit

let mult_vect m v =
  let vect_of_array a = Vect.make a.(0) a.(1) a.(2) in
  let v0 = vect_of_array m.(0) in
  let v1 = vect_of_array m.(1) in
  let v2 = vect_of_array m.(2) in
  let scalprod_with_v = Vect.scalprod v in
  Vect.make (scalprod_with_v v0) (scalprod_with_v v1) (scalprod_with_v v2)

let print m =
  let print_aux m =
    let print_line = Array.iter (fun x -> print_float x; print_char ' ') in
    Array.iter (fun l -> print_string "[ "; print_line l; print_string "] ") m
  in
  print_string "( ";
  print_aux m;
  print_char ')'
