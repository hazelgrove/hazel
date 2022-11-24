module Doc = Pretty.Doc;
open Util;
open Haz3lcore;
open DHDoc;

type formattable_child = (~enforce_inline: bool) => t;

module P = Precedence;
let precedence_const = P.max;
let precedence_Ap = P.ap;
let precedence_Times = P.mult;
let precedence_Power = P.power;
let precedence_Divide = P.mult;
let precedence_Plus = P.plus;
let precedence_Minus = P.plus;
let precedence_Cons = P.cons;
let precedence_Concat = P.concat;
let precedence_Equals = P.eqs;
let precedence_LessThan = P.eqs;
let precedence_GreaterThan = P.eqs;
let precedence_And = P.and_;
let precedence_Or = P.or_;
let precedence_Comma = P.prod;
let precedence_max = P.min;

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

  let empty_hole = ((_u, _i): HoleInstance.t): t => {
    let lbl =
      //StringUtil.cat([string_of_int(u + 1), ":", string_of_int(i + 1)]);
      "?";
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
  let colon_Fun = mk(":");
  let open_Fun = mk("{");
  let close_Fun = mk("}");

  let fix_FixF = mk("fix");
  let colon_FixF = mk(":");
  let open_FixF = mk(".{");
  let close_FixF = mk("}");

  let open_Inj = (inj_side: InjSide.t) =>
    mk(StringUtil.cat([InjSide.to_string(inj_side), "("]));
  let close_Inj = mk(")");

  let projection_dot = mk(".");

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

let mk_EmptyHole = (~selected=false, hc: HoleInstance.t) =>
  Delim.empty_hole(hc) |> Doc.annot(DHAnnot.EmptyHole(selected, hc));

let mk_ExpandingKeyword = (hc, k) =>
  Doc.text(ExpandingKeyword.to_string(k))
  |> Doc.annot(DHAnnot.VarHole(ExpandingKeyword(k), hc));

let mk_InvalidText = (t, hc) =>
  Doc.text(t) |> Doc.annot(DHAnnot.Invalid(hc));

let mk_Sequence = (doc1, doc2) => Doc.(hcats([doc1, linebreak(), doc2]));

let mk_TestLit = _n => Doc.text(ExpandingKeyword.to_string(Test));

let mk_IntLit = n => Doc.text(string_of_int(n));

let mk_StringLit = Doc.text;

let mk_FloatLit = (f: float) =>
  switch (f < 0., Float.is_infinite(f), Float.is_nan(f)) {
  | (false, true, _) => Doc.text("Inf")
  /* TODO: NegInf is temporarily introduced until unary minus is introduced to Hazel */
  | (true, true, _) => Doc.text("NegInf")
  | (_, _, true) => Doc.text("NaN")
  | _ => Doc.text(string_of_float(f))
  };

let mk_BoolLit = b => Doc.text(string_of_bool(b));

let mk_TagLit = Doc.text;

let mk_Inj = (inj_side, padded_child) =>
  Doc.hcats([Delim.open_Inj(inj_side), padded_child, Delim.close_Inj]);

let mk_Cons = (hd, tl) => Doc.(hcats([hd, text("::"), tl]));

let rec mk_comma_seq = (ld, rd, l, ol) =>
  switch (l) {
  | [] =>
    if (l == ol) {
      Doc.(hcats([text(ld), text(rd)]));
    } else {
      Doc.(hcats([text(rd)]));
    }
  | [hd, ...tl] =>
    if (l == ol) {
      Doc.(hcats([text(ld), hd, mk_comma_seq(ld, rd, tl, ol)]));
    } else {
      Doc.(hcats([text(", "), hd, mk_comma_seq(ld, rd, tl, ol)]));
    }
  };

let mk_ListLit = l => mk_comma_seq("[", "]", l, l);

let mk_Tuple = elts => mk_comma_seq("(", ")", elts, elts);

let mk_Ap = (doc1, doc2) => Doc.hseps([doc1, doc2]);

let mk_Prj = (targ, n) =>
  Doc.hcats([targ, Delim.projection_dot, Doc.text(string_of_int(n))]);
