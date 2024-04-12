open Util;
open Haz3lcore;

module Doc = Pretty.Doc;

[@deriving sexp]
type t = Doc.t(DHAnnot.t);

type formattable_child = (~enforce_inline: bool) => t;

let precedence_const = DHDoc_common.precedence_const;
let precedence_Ap = DHDoc_common.precedence_Ap;
let precedence_Times = DHDoc_common.precedence_Times;
let precedence_Divide = DHDoc_common.precedence_Divide;
let precedence_Plus = DHDoc_common.precedence_Plus;
let precedence_Minus = DHDoc_common.precedence_Minus;
let precedence_Cons = DHDoc_common.precedence_Cons;
let precedence_Equals = DHDoc_common.precedence_Equals;
let precedence_LessThan = DHDoc_common.precedence_LessThan;
let precedence_GreaterThan = DHDoc_common.precedence_GreaterThan;
let precedence_And = DHDoc_common.precedence_And;
let precedence_Or = DHDoc_common.precedence_Or;
let precedence_Comma = DHDoc_common.precedence_Comma;
let precedence_max = DHDoc_common.precedence_max;

let pad_child =
    (
      ~inline_padding as (l, r)=(Doc.empty(), Doc.empty()),
      ~enforce_inline: bool,
      child: formattable_child,
    )
    : t => {
  let inline_choice = Doc.hcats([l, child(~enforce_inline=true), r]);
  let para_choice =
    Doc.(
      hcats([
        linebreak(),
        indent_and_align(child(~enforce_inline=false)),
        linebreak(),
      ])
    );
  enforce_inline ? inline_choice : Doc.choice(inline_choice, para_choice);
};

module Delim = {
  let mk = (delim_text: string): t =>
    Doc.text(delim_text) |> Doc.annot(DHAnnot.Delim);

  let empty_hole = ((u, i): HoleInstance.t): t => {
    let lbl = StringUtil.cat([Id.to_string(u), ":", string_of_int(i + 1)]);
    Doc.text(lbl)
    |> Doc.annot(DHAnnot.HoleLabel)
    |> Doc.annot(DHAnnot.Delim);
  };

  let list_nil = mk("[]");
  let triv = mk("()");
  let wild = mk("_");

  let open_Parenthesized = mk("(");
  let close_Parenthesized = mk(")");

  let sym_Fun = mk("fun");
  let colon_Lam = mk(":");
  let open_Lam = mk(".{");
  let close_Lam = mk("}");

  let fix_FixF = mk("fix");
  let colon_FixF = mk(":");
  let open_FixF = mk(".{");
  let close_FixF = mk("}");
  let open_Case = mk("case");
  let close_Case = mk("end");

  let bar_Rule = mk("|");
  let arrow_Rule = mk("=>");

  let open_Cast = mk("<");
  let arrow_Cast = mk(Unicode.castArrowSym);
  let close_Cast = mk(">");

  let open_FailedCast = open_Cast |> Doc.annot(DHAnnot.FailedCastDelim);
  let arrow_FailedCast =
    mk(Unicode.castArrowSym) |> Doc.annot(DHAnnot.FailedCastDelim);
  let close_FailedCast = close_Cast |> Doc.annot(DHAnnot.FailedCastDelim);
};

let mk_EmptyHole = (~selected=false, (u, i)) =>
  Delim.empty_hole((u, i))
  |> Doc.annot(DHAnnot.EmptyHole(selected, (u, i)));

let mk_IntLit = n => Doc.text(string_of_int(n));

let mk_FloatLit = (f: float) =>
  switch (f < 0., Float.is_infinite(f), Float.is_nan(f)) {
  | (false, true, _) => Doc.text("Inf")
  /* TODO: NegInf is temporarily introduced until unary minus is introduced to Hazel */
  | (true, true, _) => Doc.text("NegInf")
  | (_, _, true) => Doc.text("NaN")
  | _ => Doc.text(string_of_float(f))
  };

let mk_BoolLit = b => Doc.text(string_of_bool(b));

let mk_Cons = (hd, tl) => Doc.(hcats([hd, text("::"), tl]));

let mk_Pair = (doc1, doc2) => Doc.(hcats([doc1, text(", "), doc2]));

let mk_Ap = (doc1, doc2) => Doc.hseps([doc1, doc2]);
