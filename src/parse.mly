%{
open Scenario
%}

%token<float> CONST
%token<string> IDENT
%token PROC PUT LET IF THEN ELSE OBJECT GROUP
%token AMBIENT PLANE SPHERE END ROTATION SHIFT CENTER RADIUS KD KS EOF
%token SIN COS SQRT ROTATE TRANSLATE SCALE COLOR PHONG
%token VIRG PLUS MINUS MULT DIV PARG PARD EQUAL
%token OR AND NOT LESS
%token BOX LIGHT INTENSITY LENGTH BY
%token CAMERA DISTANCE ANGLE

%left OR
%left AND
%nonassoc NOT
%left PLUS MINUS
%left MULT DIV
%left uni

%type<Scenario.scenario> scenario
%start scenario

%%

scenario:
  | c = camera a = ambient l = light* p = proc* m = instruction* EOF
      { {camera = c; ambient = a; lights = l; procs = p; main = m} }

camera:
  | CAMERA DISTANCE v = expr ANGLE a = expr END { {viewdist = v; angle = a} }

ambient: AMBIENT e = expr { e }

light:
  | LIGHT ROTATION t = triplet INTENSITY i = expr END
      { {l_dir = t; l_intensity = i} }

bexpr:
  | b1 = bexpr OR b2 = bexpr { Or(b1, b2) }
  | b1 = bexpr AND b2 = bexpr { And(b1, b2) }
  | NOT b = bexpr { Not b }
  | e1 = expr EQUAL e2 = expr { Equal(e1, e2) }
  | e1 = expr LESS e2 = expr { Less(e1, e2) }
  | PARG b = bexpr PARD { b }

expr:
  | e1 = expr PLUS e2 = expr { Bin (Plus, e1, e2) }
  | e1 = expr MINUS e2 = expr { Bin (Minus, e1, e2) }
  | e1 = expr MULT e2 = expr { Bin (Mult, e1, e2) }
  | e1 = expr DIV e2 = expr { Bin (Div, e1, e2) }
  | c = CONST { Const c }
  | i = IDENT { Ident i }
  | MINUS e = expr %prec uni { Uni (Opp, e) }
  | SQRT PARG e = expr PARD { Uni (Sqrt, e) }
  | SIN PARG e = expr PARD { Uni (Sin, e) }
  | COS PARG e = expr PARD { Uni (Cos, e) }
  | PARG e = expr PARD { e }

obj:
  | i = IDENT { Object i }
  | SPHERE CENTER c = triplet RADIUS e = expr t = texture END
      { Sphere (c, e, t) }
  | PLANE ROTATION r = triplet SHIFT e = expr t = texture END
      { Plane (r, e, t) }
  | BOX CENTER c = triplet LENGTH l = triplet t = texture END { Box (c, l, t) }
  | TRANSLATE o = obj BY t = triplet END { Translate(o, t) }
  | ROTATE o = obj BY t = triplet END { Rotate (o, t) }
  | SCALE o = obj BY e = expr END  { Scale (o, e) }
  | GROUP l = obj* END { Group l }

triplet: x = expr VIRG y = expr VIRG z = expr { (x, y, z) }

texture:
  | KD d = expr KS s = expr PHONG p = expr
    COLOR r = expr VIRG g = expr VIRG b = expr
      { {kd = d; ks = s; phong = p; color = (r, g, b)} }

instruction:
  | LET x = IDENT EQUAL e = expr { SetNum (x, e) }
  | OBJECT x = IDENT EQUAL o = obj { SetObj (x, o) }
  | f = IDENT PARG l = virg_list(expr) PARD { Call (f, l) }
  | PUT o = obj { Put o }
  | IF b = bexpr THEN i1 = instruction* ELSE i2 = instruction* END
      { If (b, i1, i2) }
  | IF b = bexpr THEN i1 = instruction* END { If (b, i1, []) }

proc:
  | PROC i = IDENT PARG p = virg_list(IDENT) PARD b = instruction* END
      { {name = i; params = p; body = b} }

virg_list(x): l = separated_list(VIRG, x) { l }
