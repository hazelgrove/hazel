let space = [%sedlex.regexp? Plus(white_space)];

// let grout_op = [%sedlex.regexp? "⬣"];
// let grout_pre = [%sedlex.regexp? "«"];
// let grout_pos = [%sedlex.regexp? "»"];
// let grout_in = [%sedlex.regexp? "⧗"];
// let grout = [%sedlex.regexp? grout_op | grout_pre | grout_pos | grout_in];

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

let pop = buf => {
  let mk = (lbl: Label.t) => {
    let text = lexeme(buf);
    let lbls = [lbl, ...Labels.completions(text)];
    Some(Token.Labeled.mk(~text, Mtrl.Tile(lbls)));
  };
  switch%sedlex (buf) {
  | space => mk(Space)
  // | grout_op => mk(Grout((Convex, Convex)))
  // | grout_pre => mk(Grout((Convex, Concave)))
  // | grout_pos => mk(Grout((Concave, Convex)))
  // | grout_in => mk(Grout((Concave, Concave)))
  | int_lit => mk(Int_lit)
  | float_lit => mk(Float_lit)
  | id_lower => mk(Id_lower)
  | id_upper => mk(Id_upper)
  | const => mk(Const(lexeme(buf)))

  | Plus(Sub(any, white_space)) =>
    let text = lexeme(buf);
    let lbls = Labels.completions(text);
    Some(Token.Labeled.mk(~text, Mtrl.Tile(lbls)));

  | eof => None

  | _ => assert(false)
  };
};

let label = (s: string): list(Token.Labeled.t) => {
  let buf = Sedlexing.Latin1.from_string(s);
  let rev = ref([]);
  let rec go = () =>
    switch (pop(buf)) {
    | None => ()
    | Some(p) =>
      rev := [p, ...rev^];
      go();
    };
  go();
  List.rev(rev^);
};
