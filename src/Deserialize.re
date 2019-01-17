open SemanticsCore;
open LangUtil;
let ensure_well_typed_after_parsing = uhexp =>
  switch (
    UHExp.fix_and_renumber_holes((), (VarCtx.empty, PaletteCtx.empty), uhexp)
  ) {
  | None => raise(IllFormed(uhexp))
  | Some(((e, t), mv)) => (e, t, mv)
  };
let parse' = lexbuf => HazelParse.parse_uhexp(HazelLex.read, lexbuf);
let deserialize = i_channel =>
  try (
    ensure_well_typed_after_parsing(parse'(Lexing.from_channel(i_channel)))
  ) {
  | HazelParse.Error => raise(InvalidSyntax("Syntax error"))
  };

let uhexp_of_string = s =>
  try (
    switch (ensure_well_typed_after_parsing(parse'(Lexing.from_string(s)))) {
    | (e, _, _) => e
    }
  ) {
  | HazelParse.Error => raise(InvalidSyntax(s))
  };
