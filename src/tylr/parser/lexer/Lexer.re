open Material;

let space = [%sedlex.regexp? Plus(white_space)];

let grout_op = [%sedlex.regexp? "⬣"]; //
let grout_pre = [%sedlex.regexp? "«"];
let grout_pos = [%sedlex.regexp? "»"];
let grout_in = [%sedlex.regexp? "⧗"];
let grout = [%sedlex.regexp? grout_op | grout_pre | grout_pos | grout_in];

let digit = [%sedlex.regexp? '0' .. '9'];
let alpha_lower = [%sedlex.regexp? 'a' .. 'z'];
let alpha_upper = [%sedlex.regexp? 'A' .. 'Z'];
let alpha = [%sedlex.regexp? alpha_lower | alpha_upper];

let int_lit = [%sedlex.regexp? Plus(digit | '_')];
let float_lit = [%sedlex.regexp?
  (Plus(digit), '.', Star(digit) | Star(digit), '.', Plus(digit))
];

let id_lower = [%sedlex.regexp?
  (alpha_lower | '_', Star(alpha | digit | '_'))
];
let id_upper = [%sedlex.regexp?
  (alpha_upper | '_', Star(alpha | digit | '_'))
];

let paren = [%sedlex.regexp? '(' | ')'];
let brack = [%sedlex.regexp? '[' | ']'];
let const = [%sedlex.regexp? paren | brack];

let labeled = [%sedlex.regexp?
  const | id_lower | id_upper | int_lit | float_lit
];

let default_tips = _ => failwith("todo");

let lexeme = Sedlexing.Latin1.lexeme;
let next_lexeme = buf => {
  let mk = m => Some(Piece.Labeled.mk(m, lexeme(buf)));
  let mk_labeled = (lbl: Label.t) => {
    let t = lexeme(buf);
    let lbls =
      Labels.const_with_prefix(t)
      |> (Label.is_const(lbl) ? Fun.id : List.cons(lbl));
    Some(Piece.Labeled.mk(Tile(lbls), t));
  };
  switch%sedlex (buf) {
  | space => mk(Space)

  | grout_op => mk(Grout((Convex, Convex)))
  | grout_pre => mk(Grout((Convex, Concave)))
  | grout_pos => mk(Grout((Concave, Convex)))
  | grout_in => mk(Grout((Concave, Concave)))

  | int_lit => mk_labeled(Int_lit)
  | float_lit => mk_labeled(Float_lit)
  | id_lower => mk_labeled(Id_lower)
  | id_upper => mk_labeled(Id_upper)
  | const => mk_labeled(Const(lexeme(buf)))

  | Plus(Sub(any, white_space)) =>
    let t = lexeme(buf);
    let lbls = Labels.with_prefix(t);
    Some(Piece.Labeled.mk(Tile(lbls), t));

  | eof => None
  | _ => assert(false)
  };
};

let lex = (s: string): list(Piece.Labeled.t) => {
  let buf = Sedlexing.Latin1.from_string(s);
  let rev = ref([]);
  let rec go = () =>
    switch (next_lexeme(buf)) {
    | None => ()
    | Some(p) =>
      rev := [p, ...rev^];
      go();
    };
  go();
  List.rev(rev^);
};

/* returns label of s if lexed as single token */
let label = s =>
  switch (lex(s)) {
  | [Tile(lbl)] => Some(lbl)
  | _ => None
  };
