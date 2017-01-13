let read_scenario f =
  let buf = open_in f in
  let lexbuf = Lexing.from_channel buf in
  try
    let (sc:Scenario.scenario) = Parse.scenario Lex.next_token lexbuf in
    close_in buf;
    sc
  with e ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "File %s, line %d, character %d: parse error\n"
      f pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    flush stderr;
    close_in buf;
    raise e

(* handle depth arg *)
let main () =
  let hsize = ref 640 in
  let vsize = ref 360 in
  let depth = ref 1 in
  let anim = ref 1 in
  let display_image scenario_filename =
    let scenario = read_scenario scenario_filename in
    Graphics.open_graph
      (" " ^ string_of_int !hsize ^ "x" ^ string_of_int !vsize);
    Graphics.set_window_title scenario_filename;

    for t = 0 to !anim - 1 do
      let scene = Scene.make scenario t in
      let img = Image.make !hsize !vsize !depth scene in
      Graphics.draw_image (Image.to_graphics img) 0 0;
      if Graphics.((wait_next_event [Poll]).keypressed) then exit 0;
    done;

    Graphics.(loop_at_exit [Key_pressed]
                (fun _ -> Graphics.close_graph (); raise Exit));
  in
  let set_if_pos, set_if_strict_pos =
    let set_if_cond cond var value = if cond value then var := value in
    set_if_cond ((<=) 0), set_if_cond ((<) 0)
  in
  let options = Arg.(
      align  [("-hsize", Int (set_if_strict_pos hsize),
               "<int> Set the width of image");
              ("-vsize", Int (set_if_strict_pos vsize),
               "<int> Set the height of image");
              ("-depth", Int (set_if_pos depth),
               "<int> Set the maximal number of bounces");
              ("-anim", Int (set_if_strict_pos anim),
               "<int> Set the number of images")]
    )
  in
  let usage_msg = Sys.argv.(0) ^ " [options] scenario_file" in
  Arg.parse options display_image usage_msg;
  exit 0

let _ =
  if not !Sys.interactive then main ()
