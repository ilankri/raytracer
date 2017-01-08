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

let main () =
  if Array.length Sys.argv = 1 then
    begin
      print_string "no scenario file given!\n";
      exit 1;
    end;
  let scenario = read_scenario Sys.argv.(1) in
  let scene = Scene.make scenario 0 in
  let width = 600 in
  let height = 400 in
  let img = Image.make width height scene in
  Graphics.open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  Graphics.draw_image (Image.to_graphics img) 0 0;
  (* TODO: handle events properly.  *)
  ignore (Graphics.wait_next_event []);
  (* print_string "Scenario file correctly read. Stop.\n"; *)
  exit 0

let _ =
  if not !Sys.interactive then main ()
