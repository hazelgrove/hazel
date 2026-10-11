/* Type-directed suggestions for TyDi, filtered to the forms that fit the type
   the cursor's position expects. One function per syntactic slot, since an
   operator, an operand and a leading form admit different candidates at the
   same `Info.t`. */

let suggest_operator: Language.Info.t => list(TyDiSuggestion.t);

let suggest_operand: Language.Info.t => list(TyDiSuggestion.t);

let suggest_leading: Language.Info.t => list(TyDiSuggestion.t);

/* The delimiter sets and the tables derived from them, exported only because
   Test_TyDi checks the derivation directly; no src/ caller. */
module Delims: {
  let leading: Language.Sort.t => list(string);
  let infix: Language.Sort.t => list(string);
  let const_mono: Language.Sort.t => list(string);
};

module Typ: {
  let deliberately_untyped: list(Language.Token.t);
  let of_const_mono_delim: Lazy.t(list((Language.Token.t, Language.Typ.t)));
  let of_infix_delim: Lazy.t(list((Language.Token.t, Language.Typ.t)));
  let of_leading_delim: Lazy.t(list((Language.Token.t, Language.Typ.t)));
};
