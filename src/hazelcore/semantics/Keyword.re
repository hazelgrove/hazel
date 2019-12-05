type t =
  | TermKW(UHExp.t, UHPat.t, HTyp.t)
  | IntermediateKW(IntermediateKeyword.t)
  | NotKW;

let is_true = x => Var.eq(x, "true");

let is_false = x => Var.eq(x, "false");

let is_let = x =>
  if (Var.eq(x, "let")) {
    Some(IntermediateKeyword.Let);
  } else {
    None;
  };

let is_case = x =>
  if (Var.eq(x, "case")) {
    Some(IntermediateKeyword.Case);
  } else {
    None;
  };

let transform_to_kw = x =>
  if (is_true(x)) {
    TermKW(
      UHExp.BoolLit(NotInHole, true),
      UHPat.BoolLit(NotInHole, true),
      HTyp.Bool,
    );
  } else if (is_false(x)) {
    TermKW(
      UHExp.BoolLit(NotInHole, false),
      UHPat.BoolLit(NotInHole, true),
      HTyp.Bool,
    );
  } else {
    switch (is_let(x)) {
    | Some(kw) => IntermediateKW(kw)
    | None =>
      switch (is_case(x)) {
      | Some(kw) => IntermediateKW(kw)
      | None => NotKW
      }
    };
  };
