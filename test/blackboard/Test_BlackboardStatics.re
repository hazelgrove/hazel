/* The checker, as the editor sees it: parse Blackboard text through the
   editor, run statics, and read the errors back out of the info map. These
   are the messages the cursor inspector shows, so a silent editor fails
   here.

   Two kinds of error must survive independently. A syntax error, where an
   entry cannot be read at all, must not silence the checking of its
   neighbours; and a check error must land on the name that caused it. */

open Alcotest;
open Haz3lcore;
open Language;

/* Every Blackboard error in a document, as (token, message) pairs. */
let errors_of = (code: string): list((string, string)) => {
  let root = Sort.Exp;
  switch (Parser.to_zipper(~root, code)) {
  | None => failf("Parser.to_zipper failed for %S", code)
  | Some(z) =>
    let term = MakeTerm.from_zip_for_sem(z, ~root).term;
    let info_map =
      fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
    let seg = Dump.to_segment(z, ~root);
    /* Walk the segment so we can name the token each error sits on. */
    let rec pieces = (seg: Segment.t): list((Id.t, string)) =>
      List.concat_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            [(t.id, String.concat("", t.label))]
            @ List.concat_map(pieces, t.children)
          | Grout(_)
          | Secondary(_)
          | Projector(_) => []
          },
        seg,
      );
    List.filter_map(
      ((id, label)) =>
        switch (Statics.Map.lookup(id, info_map)) {
        | Some(InfoBb(bb)) =>
          switch (BbInfo.error_of(bb)) {
          | Some(err) => Some((label, BbInfo.message(err)))
          | None => None
          }
        | _ => None
        },
      pieces(seg),
    );
  };
};

let tests = (
  "Blackboard statics",
  [
    test_case("a correct document reports nothing", `Quick, () =>
      check(
        list(pair(string, string)),
        "no errors",
        [],
        errors_of(
          "blackboard assume eq : (A : type) -> (a : A) -> type by tychk end",
        ),
      )
    ),
    test_case(
      "an unbound name is reported on the name itself",
      `Quick,
      () => {
        let errs =
          errors_of("blackboard construct falsity : tyXXX by definition end");
        check(
          bool,
          "tyXXX is flagged",
          true,
          List.exists(
            ((tok, msg)) => tok == "tyXXX" && msg == "unbound name tyXXX",
            errs,
          ),
        );
      },
    ),
    test_case(
      "a construct block is checked against an earlier assume",
      `Quick,
      () => {
        let ok =
          errors_of(
            "blackboard assume int : type by tychk; construct zero : int by definition end",
          );
        check(list(pair(string, string)), "int is in scope", [], ok);
        let bad =
          errors_of(
            "blackboard assume int : type by tychk; construct zero : nat by definition end",
          );
        check(
          bool,
          "nat is not",
          true,
          List.exists(((tok, _)) => tok == "nat", bad),
        );
      },
    ),
    test_case(
      "one malformed entry does not silence its neighbours",
      `Quick,
      () => {
        /* `type` alone is not an entry: it has no name. The entry after it
           must still be checked. */
        let errs =
          errors_of("blackboard assume type; bad : nope by tychk end");
        check(
          bool,
          "the malformed entry is reported",
          true,
          List.exists(((_, msg)) => msg != "unbound name nope", errs),
        );
        check(
          bool,
          "and the entry after it is still checked",
          true,
          List.exists(((_, msg)) => msg == "unbound name nope", errs),
        );
      },
    ),
  ],
);
