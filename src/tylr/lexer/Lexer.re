open Material;

let digit = [%sedlex.regexp? '0' .. '9'];

let int_lit = [%sedlex.regexp? Plus(digit | '_')];
let float_lit = [%sedlex.regexp?
  (Plus(digit), '.', Star(digit) | Star(digit), '.', Plus(digit))
];

let alpha_lower = [%sedlex.regexp? 'a' .. 'z'];
let alpha_upper = [%sedlex.regexp? 'A' .. 'Z'];
let alpha = [%sedlex.regexp? alpha_lower | alpha_upper];

let id_lower = [%sedlex.regexp?
  (alpha_lower | '_', Star(alpha | digit | '_'))
];
let id_upper = [%sedlex.regexp?
  (alpha_upper | '_', Star(alpha | digit | '_'))
];

let paren = [%sedlex.regexp? '(' | ')'];
let brack = [%sedlex.regexp? '[' | ']'];

let const = [%sedlex.regexp? paren | brack];

let next = buf =>
  switch%sedlex (buf) {
  | white_space => Some(Grout())
  | id_lower => Some(Tile(Label.Id_lower))
  | id_upper => Some(Tile(Label.Id_upper))
  | int_lit => Some(Tile(Label.Int_lit))
  | float_lit => Some(Tile(Label.Float_lit))
  | const =>
    let t = Sedlexing.Latin1.lexeme(buf);
    Some(Tile(Label.Const(t)));
  | _ => None
  };

let lex = (s: string): list(Material.Labeled.t) => {
  let buf = Sedlexing.Latin1.from_string(s);
  let rev = ref([]);
  let rec go = () =>
    switch (next(buf)) {
    | None =>
      print_endline("Lexer: unrecognized token");
      ();
    | Some(lx) =>
      rev := [lx, ...rev^];
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
