{
open Parse
open Lexing

exception Lex_error of string
}

let comment = '#' [^'\n']*

rule next_token = parse
  | ['0'-'9']+('.'['0'-'9']+)? { CONST (float_of_string (lexeme lexbuf)) }

  | "ambient" { AMBIENT }
  | "camera" { CAMERA }
  | "distance" { DISTANCE }
  | "angle" { ANGLE }
  | "object" { OBJECT }
  | "group" { GROUP }
  | "plane" { PLANE }
  | "sphere" { SPHERE }
  | "box" { BOX }
  | "light" { LIGHT }
  | "intensity" { INTENSITY }
  | "end" { END }
  | "rotation" { ROTATION }
  | "shift" { SHIFT }
  | "center" { CENTER }
  | "radius" { RADIUS }
  | "kd" { KD }
  | "ks" { KS }
  | "phong" { PHONG }
  | "sin" { SIN }
  | "cos" { COS }
  | "sqrt" { SQRT }
  | "rotate" { ROTATE }
  | "translate" { TRANSLATE }
  | "scale" { SCALE }
  | "proc" { PROC }
  | "put" { PUT }
  | "let" { LET }
  | "length" { LENGTH }
  | "by" { BY }
  | "color" { COLOR }
  | "pi" { CONST (2.0 *. asin 1.0) }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }

  | '+' { PLUS }
  | '*' { MULT }
  | '/' { DIV }
  | '-' { MINUS }
  | '(' { PARG }
  | ')' { PARD }
  | '=' { EQUAL }
  | '<' { LESS }
  | '&' { AND }
  | '|' { OR }
  | '!' { NOT }
  | ',' { VIRG }

  | ['a'-'z']+ { IDENT (lexeme lexbuf) }

  | [' ''\t''\r']+ { next_token lexbuf }
  | comment? '\n' { Lexing.new_line lexbuf; next_token lexbuf }
  | comment? eof { EOF }
  | _ { raise (Lex_error (lexeme lexbuf))  }
