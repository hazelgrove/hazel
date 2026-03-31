/**
 * This file contains tests to validate the `MakeTerm` module's ability to convert
 * zippers into expressions.
 */
open Util;
open Alcotest;
open Haz3lcore;
open Action;
module Fresh = Language.IdTagged.FreshGrammar;

/* The following special characters are used in the tests to represent
 * grout and the caret. I'd like to use extended ascii/unicode chars
 * to avoid collisions (and be prettier) but some of the below logic
 * seems to choke on fancy chars... */

let caret_char = "¦"; /* Note this is two bytes */
let convex_char = "?";
let concave_char = "~";
let selection_char = "§"; /* Note this is two bytes */
let caret_regexp = StringUtil.regexp(caret_char);

let printer = (z: Zipper.t): string => {
  Printer.of_zipper(
    ~holes=convex_char,
    ~concave_holes=concave_char,
    ~caret=caret_char,
    ~selection_anchor=selection_char,
    z,
  );
};

let default_settings = {
  ...Language.CoreSettings.off,
  statics: true,
};

let perform =
    (~settings=default_settings, zip: Zipper.t, actions: list(Action.t))
    : Zipper.t => {
  let perform = (a: Action.t, z: Zipper.t) => {
    let term = MakeTerm.from_zip_for_sem(z).term;
    let statics =
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      a,
      {
        zipper: z,
        col_target: None,
      },
    );
  };
  List.fold_left(
    (z: Zipper.t, a: Action.t) =>
      switch (perform(a, z)) {
      | Ok(z) => z
      | Error(err) =>
        print_endline("Zipper: " ++ Zipper.show(z));
        Alcotest.fail("Failed on action: " ++ Action.Failure.show(err));
      },
    zip,
    actions,
  );
};

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Token.to_list |> List.map(c => Action.Insert(c));

let mv_l = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

let mv_r = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Right, ByChar)));

let mv_l_token = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByToken)));

let mv_r_token = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Right, ByToken)));

let mk = (init: string): list(Action.t) => {
  /* Builds actions from a string with ¦ for caret position.
   * Does not support § — use mk_zipper for selections. */
  let chars = Token.to_list(init);
  if (List.exists(c => c == selection_char, chars)) {
    Alcotest.fail("mk() does not support §. Use mk_zipper(): " ++ init);
  };
  let rec split =
          (before: list(string), rest: list(string))
          : (list(string), list(string)) =>
    switch (rest) {
    | [] => Alcotest.fail("Failed to find caret in: " ++ init)
    | [hd, ...tl] =>
      if (hd == caret_char) {
        (List.rev(before), tl);
      } else {
        split([hd, ...before], tl);
      }
    };
  let (before, after) = split([], chars);
  let clean = Token.of_list(before @ after);
  let chars_after_caret = List.length(after);
  string_to_ltr_actions(clean) @ mv_l(chars_after_caret);
};

/* mk_zipper: builds a zipper with optional selection from a string.
 * Uses ¦ for caret (focus) and § for selection anchor.
 *
 * Implementation:
 * 1. Create two single-caret versions of the input string
 *    - version_a: caret at the § position (anchor)
 *    - version_b: caret at the ¦ position (focus)
 * 2. Build zippers from both via mk + perform
 * 3. Get Point.t coordinates from each via Measured
 * 4. Apply Select(PointToPoint) to create the selection */
let mk_zipper = (~settings=default_settings, init: string): Zipper.t => {
  let chars = Token.to_list(init);
  let has_anchor = List.exists(c => c == selection_char, chars);
  if (!has_anchor) {
    /* No selection — just use mk */
    mk(init) |> perform(~settings, Zipper.init());
  } else {
    /* version_a: replace § with ¦, remove original ¦ */
    let version_a =
      chars
      |> List.map(c =>
           if (c == selection_char) {
             caret_char;
           } else if (c == caret_char) {
             "";
           } else {
             c;
           }
         )
      |> List.filter(c => c != "")
      |> Token.of_list;
    /* version_b: remove §, keep ¦ */
    let version_b =
      chars |> List.filter(c => c != selection_char) |> Token.of_list;
    /* Build zippers */
    let z_a = mk(version_a) |> perform(~settings, Zipper.init());
    let z_b = mk(version_b) |> perform(~settings, Zipper.init());
    /* Get caret Points from each */
    let measured_a = CachedSyntax.init(z_a).measured;
    let measured_b = CachedSyntax.init(z_b).measured;
    let anchor_pt = Zipper.Caret.point(measured_a, z_a);
    let focus_pt = Zipper.Caret.point(measured_b, z_b);
    /* Apply PointToPoint: moves to anchor, selects to focus */
    [Action.Select(PointToPoint((anchor_pt, focus_pt)))]
    |> perform(~settings, z_b);
  };
};

/* Printer that includes indentation. mk_zipper produces Points in
 * Measured space (which includes indentation), so round-trip tests
 * must use a printer that also includes indentation for consistency. */
let printer_indented = (z: Zipper.t): string =>
  Printer.of_zipper(
    ~holes=convex_char,
    ~concave_holes=concave_char,
    ~caret=caret_char,
    ~selection_anchor=selection_char,
    ~indent=" ",
    z,
  );

let test = (~name, ~acts, ~goal): test_case(_) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> perform(Zipper.init()) |> printer,
    )
  );

let test_with_settings = (~settings, ~name, ~acts, ~goal): test_case(_) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> perform(~settings, Zipper.init()) |> printer,
    )
  );

let basic_tests = [
  test(
    ~name="Initialize caret position from string",
    ~acts=mk("¦foo"),
    ~goal="¦foo",
  ),
  test(
    ~name="Paste string duo-splitting empty tuple",
    ~acts=mk("(¦)") @ [Paste({|"foo"|})],
    ~goal={|("foo"¦)|},
  ),
  test(
    ~name="Paste string splitting token",
    ~acts=mk("1¦1") @ [Paste({|"foo"|})],
    ~goal={|1~"foo"¦~1|},
  ),
  test(
    ~name="Paste string splitting consecutive delimiters",
    ~acts=mk("if¦then") @ [Paste({|"foo"|})],
    ~goal={|if"foo"¦then?|},
  ),
  test(
    ~name="Paste string with a backpack glom false friend",
    ~acts=mk("¦") @ [Paste({|([)(|})],
    ~goal={|([?)(¦?|},
  ),
  test(
    ~name="Split two prefix op !s into bin op !!",
    ~acts=mk("--1¦") @ mv_l(2) @ [Insert(" ")],
    // caret pos is invalid here in a way not represented in these tetesl
    ~goal={|- ¦-1|},
  ),
  test(
    ~name="Delete leading constructor in sum type with prefix plus",
    ~acts=mk("1:(+A¦ +A)") @ [Destruct(Left)],
    // suceeds but crashes later with split_kids
    ~goal={|1:(+¦ +A)|},
  ),
  test(
    ~name="Split ++ op in type sort context",
    ~acts=mk({|1:(++A)¦|}) @ mv_l(3) @ [Insert(" ")],
    ~goal={|1:(+ ¦+A)|},
  ),
  test(
    ~name="Split !! infix op !s into prefix ops !]",
    ~acts=mk("!¦! X") @ [Insert(" ")],
    ~goal={|! ¦! X|},
  ),
  //wrong caret placement (and its in weird escapee mode...)
  test(
    ~name="Merge 2 prefix ops ! into infix op !!",
    ~acts=mk("! ! X¦") @ mv_l(3) @ [Destruct(Left)],
    ~goal={|?!¦! X|},
  ),
  // wrong caret placement (and its in weird escapee mode...)
  test(
    ~name="Merge + + ops in type sort context",
    ~acts=mk({|1:(+ ¦+A)|}) @ [Destruct(Left)],
    ~goal={|1:(?+¦+A)|},
  ),
];

let insertion_tests = [
  /* INSERTION : BASIC*/
  test(
    ~name="Insert whitespace",
    ~acts=mk({|¦|}) @ [Insert(" ")],
    ~goal={| ¦?|},
  ),
  test(
    ~name="Insert comment",
    ~acts=mk({|¦|}) @ [Insert("#")],
    ~goal={|#¦#?|},
  ),
  test(
    ~name="Insert string",
    ~acts=mk({|¦|}) @ [Insert({|"|})],
    ~goal={|"¦"|},
  ),
  test(
    ~name="Insert string after concave grout",
    ~acts=mk({|1 ¦|}) @ [Insert({|"|})],
    ~goal={|1 ~"¦"|},
  ),
  test(
    ~name="Insert char at end of token",
    ~acts=mk({|fo¦|}) @ [Insert("o")],
    ~goal={|foo¦|},
  ),
  test(
    ~name="Insert char at start of token",
    ~acts=mk({|¦oo|}) @ [Insert("f")],
    ~goal={|f¦oo|},
  ),
  test(
    ~name="Insert char inside token",
    ~acts=mk({|fi¦me|}) @ [Insert("x")],
    ~goal={|fix¦me|},
  ),
  test(
    ~name="Inserting string quote inserts closing quote as well",
    ~acts=mk({|¦|}) @ [Insert("\"")],
    ~goal={|"¦"|},
  ),
  test(
    ~name="Paste emoji inside string",
    ~acts=mk({|"¦"|}) @ [Paste("😄")],
    ~goal={|"😄¦"|},
  ),
  test(
    ~name="Insert char before emoji",
    ~acts=mk({|"¦😄"|}) @ [Insert("x")],
    ~goal={|"x¦😄"|},
  ),
  test(
    ~name="Insert char after emoji",
    ~acts=mk({|"😄¦"|}) @ [Insert("x")],
    ~goal={|"😄x¦"|},
  ),
  test(
    ~name="Insert emoji before emoji",
    ~acts=mk({|"¦😄"|}) @ [Insert("😊")],
    ~goal={|"😊¦😄"|},
  ),
  test(
    ~name="Insert emoji after emoji",
    ~acts=mk({|"😄¦"|}) @ [Insert("😊")],
    ~goal={|"😄😊¦"|},
  ),
  /* INSERTION: GROUT/SPACE TRANSMUTATION */
  /* Prefixes of trailing delimiters get a special concave
     mold option to avoid grout insertion jank during entry */
  test(
    ~name="Delimiter prefix molding 1",
    ~acts=mk({|let a = 1 ¦|}) @ [Insert("i")],
    ~goal={|let a = 1 i¦?|},
  ),
  /* Then, when you drop the n, the concave grout that appears in the
     above case should disappear, leaving a space */
  test(
    ~name="Delimiter prefix molding 2",
    ~acts=mk({|let a = 1 i¦|}) @ [Insert("n")],
    ~goal={|let a = 1 in¦?|},
  ),
  test(
    ~name="Delimiter prefix molding 3",
    ~acts=mk({|let a = 1 in¦|}) @ [Insert(" ")],
    ~goal={|let a = 1 in ¦?|},
  ),
  /* INSERTION: SUPPRESSED SPACE TRACKING */
  /* Space suppression tracking: space reappears when grout is consumed */
  test(
    ~name="Suppressed space reappears on grout fill",
    ~acts=mk({|1¦1|}) @ [Insert(" "), Insert("+")],
    ~goal={|1 +¦1|},
  ),
  /* INSERTION: TOKEN SPLITTING */
  test(
    ~name="Split empty list",
    ~acts=mk({|[¦]|}) @ [Insert(" ")],
    ~goal={|[?¦]|},
  ),
  test(
    ~name="Split case end",
    ~acts=mk({|case¦end|}) @ [Insert(" ")],
    ~goal={|case?¦end|},
  ),
  test(
    ~name="Split number literal",
    ~acts=mk({|1¦1|}) @ [Insert(" ")],
    ~goal={|1~¦1|},
  ),
  /* Spliting tokens when the latter must drop from backpack */
  test(
    ~name="Split 1st and 2nd delims of 3-delim form with space",
    ~acts=mk({|if¦then|}) @ [Insert(" ")],
    ~goal={|if?¦then?|},
  ),
  test(
    ~name="Split mono child and 2nd delim of 3-delim form",
    ~acts=mk({|if true¦then|}) @ [Insert(" ")],
    ~goal={|if true ¦then?|},
  ),
  test(
    ~name="Split 1st and 2nd delims of 3-delim form with instant expander",
    ~acts=mk({|if¦then|}) @ [Insert("(")],
    ~goal={|if(?¦then?|},
  ),
  /* Spliting tokens when both must expand */
  test(
    ~name="Split two leading delated expander delims with bin op",
    ~acts=mk({|if¦if|}) @ [Insert("+")],
    ~goal={|if?+¦if?|},
  ),
  /* The next three tests cover issue #1907. They are slightly awkwardly
     written; the details don't matter so much here. The important thing
     is in this situation we are likely wanting to wrap the existing form,
     so we want the rightwards leading token to match the existing
     delimiters, not the leftwards one. */
  test(
    ~name="Inserting if before existing if doesn't steal delimiters",
    ~acts=
      mk({|¦if 1 then 2 else 3|})
      @ [Insert("i"), Insert("f"), Insert(" "), Put_down, Put_down],
    ~goal={|if? then?else¦if 1 then 2 else 3|},
  ),
  test(
    ~name="Inserting let before existing let doesn't steal delimiters",
    ~acts=
      mk({|¦let x = 2 in 3|})
      @ [
        Insert("l"),
        Insert("e"),
        Insert("t"),
        Insert(" "),
        Put_down,
        Put_down,
      ],
    ~goal={|let? =?in¦let x = 2 in 3|},
  ),
  test(
    ~name="Inserting let before existing type doesn't steal delimiters",
    ~acts=
      mk({|¦type x = 2 in 3|})
      @ [
        Insert("l"),
        Insert("e"),
        Insert("t"),
        Insert(" "),
        Put_down,
        Put_down,
      ],
    ~goal={|let? =?in¦type x = 2 in 3|},
  ),
  /* Below test is slightly precious. Can't directly write
     `if then¦else` as then will instantly expand, so need
     to do this indirectly. The space after the first hole
     isn't perfect but it'll do for now */
  test(
    ~name="Split 2nd delim of 3-delim form with space",
    ~acts=mk({|if the¦else|}) @ [Insert("n"), Insert(" ")],
    ~goal={|if? then?¦else?|},
  ),
  /* INSERTTION: AMPHIBIOUS PREFIX/INFIX OP */
  test(
    ~name="Amphibious Plus 0",
    ~acts=mk({|type T = A ¦|}),
    ~goal={|type T = A ¦|},
  ),
  test(
    ~name="Amphibious Plus - At End - 1",
    ~acts=mk({|type T = A ¦|}) @ [Insert("+")],
    ~goal={|type T = A +¦?|},
  ),
  test(
    ~name="Amphibious Plus - At End - 2",
    ~acts=mk({|type T = A + B + ¦|}) @ [Insert("C")],
    ~goal={|type T = A + B + C¦|},
  ),
  test(
    ~name="Amphibious Plus - At End - 3",
    ~acts=mk({|type T = + ¦|}),
    ~goal={|type T = + ¦?|},
  ),
  test(
    ~name="Amphibious Plus - At End - 4",
    ~acts=mk({|type T = + ¦|}) @ [Insert("A")],
    ~goal={|type T = + A¦|},
  ),
  test(
    ~name="Amphibious Plus - At End - 5",
    ~acts=mk({|type T = + A + B + C¦|}),
    ~goal={|type T = + A + B + C¦|},
  ),
  test(
    ~name="Amphibious Plus - Before - 1",
    ~acts=mk({|type T = ¦A|}) @ [Insert("+")],
    ~goal={|type T = +¦A|},
  ),
  test(
    ~name="Amphibious Plus - Before - 2",
    ~acts=mk({|type T = ¦+ B|}) @ [Insert("A")],
    ~goal={|type T = A¦+ B|},
  ),
  test(
    ~name="Amphibious Plus - Before - 3 (Prelude)",
    ~acts=mk({|type T = A ¦ B|}),
    ~goal={|type T = A  ¦~B|},
  ),
  test(
    ~name="Amphibious Plus - Before - 3",
    ~acts=mk({|type T = A ¦ B|}) @ [Insert("+")],
    ~goal={|type T = A  +¦B|},
  ),
  test(
    ~name="Amphibious Plus - Before - 4",
    ~acts=mk({|type T = ¦ + A + B|}) @ [Insert("+")],
    ~goal={|type T = +¦ + A + B|},
  ),
  /* DROPPING */
  test(
    ~name="Insert between non-leading delims when leading in backpack",
    ~acts=
      mk({|if¦ 1 then 2 else 3|})
      @ [Destruct(Left), Destruct(Left)]
      @ mv_r(8)
      @ [Insert(" ")],
    ~goal={| 1 then  ¦2 else 3|},
  ),
  test(
    ~name="Insert let binding before prefix negation",
    ~acts=
      mk({|¦-1|})
      @ [
        Insert("l"),
        Insert("e"),
        Insert("t"),
        Insert(" "),
        Insert("x"),
        Insert(" "),
        Insert("="),
        Insert(" "),
        Move(Local(Right, ByChar)),
      ],
    ~goal={|let x = -¦1|},
  ),
  /* INSERTING WITHIN/ADJACENT TO POLYTILE DELIMITERS */
  /* Some of the below grout placements feel inconsistent...
   * it's okay if they change */
  test(
    ~name="Prepending to leading delimiter: if",
    ~acts=mk({|¦if 1 then 2 else 3|}) @ [Insert("x")],
    ~goal={|x¦~if 1 then 2 else 3|},
  ),
  test(
    ~name="Prepending to middle delimiter: if",
    ~acts=mk({|if 1 ¦then 2 else 3|}) @ [Insert("x")],
    ~goal={|if 1 ~x¦then~ 2 else 3|},
  ),
  test(
    ~name="Prepending to trailing delimiter: if",
    ~acts=mk({|if 1 then 2 ¦else 3|}) @ [Insert("x")],
    ~goal={|if 1 then 2 ~x¦else~ 3|},
  ),
  test(
    ~name="Within leading delimiter: if",
    ~acts=mk({|i¦f 1 then 2 else 3|}) @ [Insert("x")],
    ~goal={|ix¦f~ 1 then 2 else 3|},
  ),
  test(
    ~name="Within middle delimiter: if",
    ~acts=mk({|if 1 th¦en 2 else 3|}) @ [Insert("x")],
    ~goal={|if 1~ thx¦en~ 2 else 3|},
  ),
  test(
    ~name="Within trailing delimiter: if",
    ~acts=mk({|if 1 then 2 e¦lse 3|}) @ [Insert("x")],
    ~goal={|if 1 then 2~ ex¦lse~ 3|},
  ),
  test(
    ~name="Postpending to leading delimiter: if",
    ~acts=mk({|if¦ 1 then 2 else 3|}) @ [Insert("x")],
    ~goal={|ifx¦~ 1 then 2 else 3|},
  ),
  test(
    ~name="Postpending to middle delimiter: if",
    ~acts=mk({|if 1 then¦ 2 else 3|}) @ [Insert("x")],
    ~goal={|if 1 ~thenx¦~ 2 else 3|},
  ),
  test(
    ~name="Postpending to trailing delimiter: if",
    ~acts=mk({|if 1 then 2 else¦ 3|}) @ [Insert("x")],
    ~goal={|if 1 then 2 ~elsex¦~ 3|},
  ),
  test(
    ~name="Grout inserted on correct side of caret when adding if before",
    ~acts=mk({|¦[]|}) @ [Insert("i")],
    ~goal={|i¦~[]|},
  ),
  /* SPLITTING */
  test(
    ~name="Split ap (Make sure outside gets remolded)",
    ~acts=mk({|ap(¦)|}) @ [Insert(" ")],
    ~goal={|ap(?¦)|},
  ),
  /* MERGING */
  test(
    ~name="Prelude for: Merge across concave grout on insert",
    ~acts=mk({|if 1 then 2 e¦lse 3|}) @ [Destruct(Left)],
    ~goal={|if 1 then 2 ¦~lse~ 3|},
  ),
  test(
    ~name="Merge across concave grout on insert",
    ~acts=mk({|if 1 then 2 e¦lse 3|}) @ [Destruct(Left), Insert("e")],
    ~goal={|if 1 then 2 e¦lse 3|},
  ),
  test(
    ~name="Nested parens edge case (See Insert.parens_edge_case)",
    ~acts=mk({|f(g¦)|}) @ [Insert("("), Insert(")")],
    ~goal={|f(g()¦)|},
  ),
  test(
    ~name="Issue #1914 regression test",
    ~acts=mk({|((1)¦|}) @ [Put_down],
    ~goal={|((1))¦|},
  ),
  test(
    ~name=
      "Poly (formerly Forall) regrouting edge case (debatable behavior) (#1913)",
    ~acts=mk({|?:pol¦(?)|}) @ [Insert("y")],
    ~goal={|?:poly¦(?)|},
  ),
  test(
    ~name=
      "Poly (formerly Forall) regrouting edge case (non-debatable) (#1913)",
    ~acts=mk({|?:pol¦(?)|}) @ [Insert("y"), Insert("-"), Insert(">")],
    ~goal={|?:poly?->¦(?)|},
  ),
  /* In below test, we first cause the two `=`s to merge, then split them.
     The first `=` should not get matched to the `let` because of the parens.
     If it does, then it will prevent the Put_down from dropping the parens.
     This was previously causes by the misssing ancestor shards being in the
     local backpack in front of the missing sibling shards, so if the `let`
     and `in` are down, but their `=` is up, the `=` would appear before
     the `(` in the local_missing_shards.  */
  test(
    ~name="Split paren rematch (Regression guard for #1948)",
    ~acts=
      mk({|let(a=1)¦= 1 in 1|})
      @ [Destruct(Left), Destruct(Left), Insert("1"), Put_down],
    ~goal={|let(a=1)¦= 1 in 1|},
  ),
  /* DELIMITER REASSOCIATION */
  /* Test that the rescan doesn't incorrectly convert standalone operators
   * that are shadowed by a closer incomplete tile's scope */
  test(
    ~name="Labeled tuple = inside parens not stolen by let",
    ~acts=mk({|let x : (l=String) = ("a") in x¦|}),
    ~goal={|let x : (l=String) = ("a") in x¦|},
  ),
  /* Test fun with parens and arrow */
  test(
    ~name="Fun with parenthesized pattern",
    ~acts=mk({|fun (a, b) -> x¦|}),
    ~goal={|fun (a, b) -> x¦|},
  ),
  /* Test that typing `fun` before a standalone `->` reassociates them.
   * Type `-> x`, move to start, type `fun a `. The rescan should convert
   * the standalone `->` to fun's trailing delimiter. */
  test(
    ~name="Rescan: fun typed before existing standalone ->",
    ~acts=mk({|-> x¦|}) @ mv_l(4) @ string_to_ltr_actions("fun a "),
    ~goal={|fun a ¦-> x|},
  ),
  /* REMOLDING REGRESSION: inserting ( to split a token into
   * function application should not leave concave grout.
   * Bug: `string_capitalize(1)` gets concave grout before `(`.
   * The paren insertion splits the token but remolding should
   * eliminate the need for grout between the function and its arg. */
  test(
    ~name="Insert ( to split fn application (no concave grout)",
    ~acts=mk({|string_capitalize¦1)|}) @ [Insert("(")],
    ~goal={|string_capitalize(¦1)|},
  ),
];

let destruct_tests = [
  /* DESTRUCTION: BASIC */
  test(
    ~name="Delete comment",
    ~acts=mk({|##¦|}) @ [Destruct(Left)],
    ~goal={|¦?|},
  ),
  test(
    ~name="Delete string",
    ~acts=mk({|""¦|}) @ [Destruct(Left)],
    ~goal={|¦?|},
  ),
  test(
    ~name="Deleting comment delimiter deletes comment",
    ~acts=mk({|#¦#|}) @ [Destruct(Left)],
    ~goal={|¦?|},
  ),
  test(
    ~name="Deleting string delimiter deletes string",
    ~acts=mk({|"¦"|}) @ [Destruct(Left)],
    ~goal={|¦?|},
  ),
  test(
    ~name="Delete char from token by backspacing",
    ~acts=mk({|f¦oo|}) @ [Destruct(Left)],
    ~goal={|¦oo|},
  ),
  test(
    ~name="Merge to empty list by backspacing",
    ~acts=mk({|[1¦]|}) @ [Destruct(Left)],
    ~goal={|[¦]|},
  ),
  test(
    ~name="Merge to empty tuple by deleting",
    ~acts=mk({|(¦1)|}) @ [Destruct(Right)],
    ~goal={|(¦)|},
  ),
  test(
    ~name="Merge number literals across bin op by backspacing",
    ~acts=mk({|1+¦1|}) @ [Destruct(Left)],
    ~goal={|1¦1|},
  ),
  /* DESTRUCTION: MATCHING */
  test(
    ~name="Destruct leading delim in convex 2-form",
    ~acts=mk({|(¦1)|}) @ [Destruct(Left)],
    ~goal={|¦1)|},
  ),
  test(
    ~name="Destruct leading delim in prefix 3-form",
    ~acts=mk({|if¦ 1 then 2 else 3|}) @ [Destruct(Left), Destruct(Left)],
    ~goal={|¦ 1 then 2 else 3|},
  ),
  /* DESTRUCTION: AMPHIBIOUS PREFIX/INFIX OP */
  test(
    ~name="Amphibious Plus Destruct 1",
    ~acts=mk({|type T = A +¦|}) @ [Destruct(Left)],
    ~goal={|type T = A ¦|},
  ),
  test(
    ~name="Amphibious Plus Destruct 2",
    ~acts=mk({|type T = A + B +¦|}) @ [Destruct(Left)],
    ~goal={|type T = A + B ¦|},
  ),
  test(
    ~name="Amphibious Plus Destruct 3",
    ~acts=mk({|type T = A + B + C¦|}) @ [Destruct(Left)],
    ~goal={|type T = A + B + ¦?|},
  ),
  test(
    ~name="Amphibious Plus Destruct 4",
    ~acts=mk({|type T = + A¦|}) @ [Destruct(Left)],
    ~goal={|type T = + ¦?|},
  ),
  test(
    ~name="Amphibious Plus Destruct 5",
    ~acts=mk({|type T = + A +¦|}) @ [Destruct(Left)],
    ~goal={|type T = + A ¦|},
  ),
  test(
    ~name="Amphibious Plus Destruct 6",
    ~acts=mk({|type T = + A + B¦|}) @ [Destruct(Left)],
    ~goal={|type T = + A + ¦?|},
  ),
  test(
    ~name="Amphibious Plus Destruct 7",
    ~acts=mk({|type T = + A + B +¦|}) @ [Destruct(Left)],
    ~goal={|type T = + A + B ¦|},
  ),
  test(
    ~name="Amphibious Plus Destruct 8",
    ~acts=mk({|type T = +¦ A + B + C|}) @ [Destruct(Left)],
    ~goal={|type T = ¦ A + B + C|},
  ),
  test(
    ~name="Amphibious Plus Destruct 8",
    ~acts=mk({|type T = + A¦ + B + C|}) @ [Destruct(Left)],
    /* Ideally this would have a hole but okay-ish */
    ~goal={|type T = + ¦ + B + C|},
  ),
  test(
    ~name="Amphibious Plus Destruct 9",
    ~acts=mk({|type T = + A + B +¦ C|}) @ [Destruct(Left)],
    ~goal={|type T = + A + B ¦~ C|},
  ),
  test(
    ~name="Amphibious Plus Destruct 10",
    ~acts=mk({|type T = + A + B¦ + C|}) @ [Destruct(Left)],
    /* Ideally this would have a hole but okay-ish */
    ~goal={|type T = + A + ¦ + C|},
  ),
  /* Regressions */
  test(
    ~name="Regrouting edge case 1",
    ~acts=mk({|if 1then else¦|}) @ mv_l(9) @ [Insert(" ")],
    ~goal={|if 1 ¦then? else?|},
  ),
  test(
    ~name="Regrouting edge case 2",
    ~acts=mk({|if thena¦ else|}) @ [Destruct(Left)],
    ~goal={|if? then¦? else?|},
  ),
  /* If the below fails, it's likely zipper.caret isn't being
   * properly updated during insert/delete actions */
  test(
    ~name="Inner Caret position maintenance",
    ~acts=
      mk({|if 1 the¦n|}) @ [Insert(" "), Destruct(Left), Destruct(Left)],
    ~goal={|if 1 ~th¦n|},
  ),
  /* DELETING WITHIN/ADJACENT TO POLYTILE DELIMITERS */
  /* Some of the below grout placements feel inconsistent...
   * it's okay if they change */
  test(
    ~name="Within leading delimiter: if",
    ~acts=mk({|i¦f 1 then 2 else 3|}) @ [Destruct(Left)],
    ~goal={|¦f~ 1 then 2 else 3|},
  ),
  test(
    ~name="Within middle delimiter: if",
    ~acts=mk({|if 1 th¦en 2 else 3|}) @ [Destruct(Left)],
    ~goal={|if 1 ~t¦en~ 2 else 3|},
  ),
  test(
    ~name="Within trailing delimiter: if",
    ~acts=mk({|if 1 then 2 e¦lse 3|}) @ [Destruct(Left)],
    ~goal={|if 1 then 2 ¦~lse~ 3|},
  ),
  test(
    ~name="At end of leading delimiter: if",
    ~acts=mk({|if¦ 1 then 2 else 3|}) @ [Destruct(Left)],
    ~goal={|i¦~ 1 then 2 else 3|},
  ),
  test(
    ~name="At end of middle delimiter: if",
    ~acts=mk({|if 1 then¦ 2 else 3|}) @ [Destruct(Left)],
    ~goal={|if 1 the¦ 2 else 3|} /* No grout bc delim prefix special case */
  ),
  test(
    ~name="At end of trailing delimiter: if",
    ~acts=mk({|if 1 then 2 else¦ 3|}) @ [Destruct(Left)],
    ~goal={|if 1 then 2 els¦ 3|},
  ),
  test(
    ~name="Delete emoji inside string",
    ~acts=mk({|"😄¦😊"|}) @ [Destruct(Left)],
    ~goal={|"¦😊"|},
  ),
  test(
    ~name="Delete emoji at start of string",
    ~acts=mk({|"😄¦a"|}) @ [Destruct(Left)],
    ~goal={|"¦a"|},
  ),
  test(
    ~name="Delete emoji at end of string",
    ~acts=mk({|"a😄¦"|}) @ [Destruct(Left)],
    ~goal={|"a¦"|},
  ),
];

let move_tests = [
  // ByToken Right Complete Syntax
  test(
    ~name="Caret movement by token 1",
    ~acts=mk({|¦let foo = 1 in foo|}) @ mv_r_token(1),
    ~goal={|let¦ foo = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token 2",
    ~acts=mk({|let¦ foo = 1 in foo|}) @ mv_r_token(1),
    ~goal={|let ¦foo = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token 3",
    ~acts=mk({|let ¦foo = 1 in foo|}) @ mv_r_token(1),
    ~goal={|let foo¦ = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token 4",
    ~acts=mk({|let foo¦ = 1 in foo|}) @ mv_r_token(1),
    ~goal={|let foo ¦= 1 in foo|},
  ),
  test(
    ~name="Caret movement by token 5",
    ~acts=mk({|let foo ¦= 1 in foo|}) @ mv_r_token(1),
    ~goal={|let foo =¦ 1 in foo|},
  ),
  test(
    ~name="Caret movement by token 6",
    ~acts=mk({|let foo =¦ 1 in foo|}) @ mv_r_token(1),
    ~goal={|let foo = ¦1 in foo|},
  ),
  test(
    ~name="Caret movement by token 7",
    ~acts=mk({|let foo = ¦1 in foo|}) @ mv_r_token(1),
    ~goal={|let foo = 1¦ in foo|},
  ),
  test(
    ~name="Caret movement by token 8",
    ~acts=mk({|let foo = 1¦ in foo|}) @ mv_r_token(1),
    ~goal={|let foo = 1 ¦in foo|},
  ),
  test(
    ~name="Caret movement by token 9",
    ~acts=mk({|let foo = 1 ¦in foo|}) @ mv_r_token(1),
    ~goal={|let foo = 1 in¦ foo|},
  ),
  test(
    ~name="Caret movement by token 10",
    ~acts=mk({|let foo = 1 in¦ foo|}) @ mv_r_token(1),
    ~goal={|let foo = 1 in ¦foo|},
  ),
  test(
    ~name="Caret movement by token 11",
    ~acts=mk({|let foo = 1 in ¦foo|}) @ mv_r_token(1),
    ~goal={|let foo = 1 in foo¦|},
  ),
  // ByToken Left Complete Syntax
  test(
    ~name="Caret movement by token Left 1",
    ~acts=mk({|let foo = 1 in foo¦|}) @ mv_l_token(1),
    ~goal={|let foo = 1 in ¦foo|},
  ),
  test(
    ~name="Caret movement by token Left 2",
    ~acts=mk({|let foo = 1 in ¦foo|}) @ mv_l_token(1),
    ~goal={|let foo = 1 in¦ foo|},
  ),
  test(
    ~name="Caret movement by token Left 3",
    ~acts=mk({|let foo = 1 in¦ foo|}) @ mv_l_token(1),
    ~goal={|let foo = 1 ¦in foo|},
  ),
  test(
    ~name="Caret movement by token Left 4",
    ~acts=mk({|let foo = 1 ¦in foo|}) @ mv_l_token(1),
    ~goal={|let foo = 1¦ in foo|},
  ),
  test(
    ~name="Caret movement by token Left 5",
    ~acts=mk({|let foo = 1¦ in foo|}) @ mv_l_token(1),
    ~goal={|let foo = ¦1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 6",
    ~acts=mk({|let foo = ¦1 in foo|}) @ mv_l_token(1),
    ~goal={|let foo =¦ 1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 7",
    ~acts=mk({|let foo =¦ 1 in foo|}) @ mv_l_token(1),
    ~goal={|let foo ¦= 1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 8",
    ~acts=mk({|let foo ¦= 1 in foo|}) @ mv_l_token(1),
    ~goal={|let foo¦ = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 9",
    ~acts=mk({|let foo¦ = 1 in foo|}) @ mv_l_token(1),
    ~goal={|let ¦foo = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 10",
    ~acts=mk({|let ¦foo = 1 in foo|}) @ mv_l_token(1),
    ~goal={|let¦ foo = 1 in foo|},
  ),
  test(
    ~name="Caret movement by token Left 11",
    ~acts=mk({|let¦ foo = 1 in foo|}) @ mv_l_token(1),
    ~goal={|¦let foo = 1 in foo|},
  ),
  // ByToken Escapes inside if starts inside token
  test(
    ~name="ByToken escapes token left",
    ~acts=mk({|foo¦bar|}) @ mv_l_token(1),
    ~goal={|¦foobar|},
  ),
  test(
    ~name="ByToken escapes token right",
    ~acts=mk({|foo¦bar|}) @ mv_r_token(1),
    ~goal={|foobar¦|},
  ),
  // ByChar Complete Syntax
  test(
    ~name="Caret movement 3-delim R 1",
    ~acts=mk({|¦if 1 then 2 else 3|}) @ mv_r(1),
    ~goal={|i¦f 1 then 2 else 3|},
  ),
  test(
    ~name="Caret movement 3-delim R 2",
    ~acts=mk({|if 1 ¦then 2 else 3|}) @ mv_r(1),
    ~goal={|if 1 t¦hen 2 else 3|},
  ),
  test(
    ~name="Caret movement 3-delim R 3",
    ~acts=mk({|if 1 the¦n 2 else 3|}) @ mv_r(1),
    ~goal={|if 1 then¦ 2 else 3|},
  ),
  test(
    ~name="Caret movement 3-delim L 1",
    ~acts=mk({|if 1 then¦ 2 else 3|}) @ mv_l(1),
    ~goal={|if 1 the¦n 2 else 3|},
  ),
  test(
    ~name="Caret movement 3-delim L 2",
    ~acts=mk({|if 1 th¦en 2 else 3|}) @ mv_l(1),
    ~goal={|if 1 t¦hen 2 else 3|},
  ),
  test(
    ~name="Caret movement 3-delim L 3",
    ~acts=mk({|if 1 t¦hen 2 else 3|}) @ mv_l(1),
    ~goal={|if 1 ¦then 2 else 3|},
  ),
  test(
    ~name="Caret movement takes into account which shards are down - Right",
    ~acts=
      mk({|if¦ 1 then 2 else 3|})
      @ [Destruct(Left), Destruct(Left), ...mv_r(4)],
    ~goal={| 1 t¦hen 2 else 3|},
  ),
  test(
    ~name="Caret movement takes into account which shards are down - Left",
    ~acts=
      mk({|if¦ 1 then 2 else 3|})
      @ [Destruct(Left), Destruct(Left)]
      @ mv_r(7)
      @ mv_l(1),
    ~goal={| 1 the¦n 2 else 3|},
  ),
];

let selection_tests = [
  /* mk_zipper round-trip tests. Use printer_indented because Points
   * from Measured include indentation columns. */
  test_case(
    "mk_zipper: single-line anchor left",
    `Quick,
    () => {
      let z = mk_zipper({|let x = §1 in¦ x|});
      let goal = {|let x = §1 in¦ x|};
      check(
        testable(Fmt.string, String.equal),
        goal,
        goal,
        printer_indented(z),
      );
    },
  ),
  test_case(
    "mk_zipper: single-line anchor right",
    `Quick,
    () => {
      let z = mk_zipper({|let x = ¦1 in§ x|});
      let goal = {|let x = ¦1 in§ x|};
      check(
        testable(Fmt.string, String.equal),
        goal,
        goal,
        printer_indented(z),
      );
    },
  ),
  test_case(
    "mk_zipper: multi-line let selection",
    `Quick,
    () => {
      let z = mk_zipper({|let x = 1 in
§let y = 2 in¦
x + y|});
      let goal = {|let x = 1 in
§let y = 2 in¦
x + y|};
      check(
        testable(Fmt.string, String.equal),
        goal,
        goal,
        printer_indented(z),
      );
    },
  ),
  test_case(
    "mk_zipper: multi-line if/then/else selection",
    `Quick,
    () => {
      let z = mk_zipper({|if a then
§fun b ->
if c then d ¦else e
else f|});
      let result = printer_indented(z);
      /* Check selection was created (has both markers) */
      let has_anchor =
        List.exists(c => c == selection_char, Token.to_list(result));
      let has_caret =
        List.exists(c => c == caret_char, Token.to_list(result));
      if (!has_anchor || !has_caret) {
        Alcotest.fail("mk_zipper failed to create selection");
      };
    },
  ),
  test(
    ~name="Move to right from selection",
    ~acts=
      mk({|¦(1,2,3,4,5)|}) @ [Action.Select(Term(Current))] @ mv_r(1),
    ~goal={|(1,2,3,4,5)¦|},
  ),
  test(
    ~name="Select term with selection",
    ~acts=mk({|¦(1,2,3,4,5)|}) @ [Action.Select(Term(Current))],
    ~goal={|§(1,2,3,4,5)¦|},
  ),
  test(
    ~name="Select term from right",
    ~acts=mk({|(1,2,3,4,5)¦|}) @ [Select(Term(Current))],
    ~goal={|§(1,2,3,4,5)¦|},
  ),
  test(
    ~name="Select subterm with selection",
    ~acts=mk({|(1 + (2 ¦+ 3)|}) @ [Action.Select(Term(Current))],
    ~goal={|(1 + (§2 + 3¦)|},
  ),
  test(
    ~name="Select term with let binding does not select body",
    ~acts=mk({|¦let x = 1 in x|}) @ [Action.Select(Term(Current))],
    ~goal={|§let x = 1 in¦ x|},
  ),
  test(
    ~name="Select term when on comma in tuple selects whole tuple",
    ~acts=
      mk({|let x = 1 in (x, 1,¦ ?)|}) @ [Action.Select(Term(Current))],
    ~goal={|let x = 1 in §(x, 1, ?)¦|},
  ),
  test(
    ~name="Move to left from selection starting at left",
    ~acts=
      mk({|¦(1,2,3,4,5)|}) @ [Action.Select(Term(Current))] @ mv_l(1),
    ~goal={|¦(1,2,3,4,5)|},
  ),
  test(
    ~name="Move to left from selection starting at right",
    ~acts=
      mk({|(1,2,3,4,5)¦|}) @ [Action.Select(Term(Current))] @ mv_l(1),
    ~goal={|¦(1,2,3,4,5)|},
  ),
  test(
    ~name="Move to right from selection starting at right",
    ~acts=
      mk({|(1,2,3,4,5)¦|}) @ [Action.Select(Term(Current))] @ mv_r(1),
    ~goal={|(1,2,3,4,5)¦|},
  ),
  test(
    ~name="Move to right from selection starting at right",
    ~acts=
      mk({|¦(1,2,3,4,5)|}) @ [Action.Select(Term(Current))] @ mv_r(1),
    ~goal={|(1,2,3,4,5)¦|},
  ),
  test(
    ~name="ht by token from selection",
    ~acts=
      mk({|(1, ¦(2, 3), 4, 5)|})
      @ [Action.Select(Term(Current))]
      @ mv_r_token(1),
    ~goal={|(1, (2, 3),¦ 4, 5)|},
  ),
  test(
    ~name="Move left by token from selection",
    ~acts=
      mk({|(1, ¦(2, 3), 4, 5)|})
      @ [Action.Select(Term(Current))]
      @ mv_l_token(1),
    ~goal={|(1,¦ (2, 3), 4, 5)|},
  ),
  test(
    ~name="Move left by token when selecting everything",
    ~acts=mk({|(1, 2,¦ 3, 4)|}) @ [Action.Select(All)] @ mv_l_token(1),
    ~goal={|¦(1, 2, 3, 4)|},
  ),
  test(
    ~name="Move right by token when selecting everything",
    ~acts=mk({|(1, 2,¦ 3, 4)|}) @ [Action.Select(All)] @ mv_r_token(1),
    ~goal={|(1, 2, 3, 4)¦|},
  ),
  test(
    ~name="Move extreme left with multiline selection",
    ~acts=
      mk({|(12345,
  23456789,
  ¦345678,
  45678,
  56789)|})
      @ [Action.Select(All)]
      @ [Action.Move(Line(Left))],
    ~goal={|(12345,
  23456789,
  345678,
  45678,
  ¦56789)|},
  ),
  test(
    ~name="Extend selection left by token",
    ~acts=
      mk({|let x = 1 in (x, 12345¦, ?)|})
      @ [Action.Select(Resize(Local(Left, ByToken)))],
    ~goal={|let x = 1 in (x, ¦12345§, ?)|},
  ),
  /* --- Double-click (Smart(2)) and triple-click (Smart(3)) tests --- */
  test(
    ~name="Double-click on multi-char infix: select &&",
    ~acts=mk({|true ¦&& false|}) @ [Action.Select(Smart(2))],
    ~goal={|true §&&¦ false|},
  ),
  test(
    ~name="Double-click on single-char infix: select +",
    ~acts=mk({|1 ¦+ 2|}) @ [Action.Select(Smart(2))],
    ~goal={|1 §+¦ 2|},
  ),
  test(
    ~name="Double-click on operand: select operand",
    ~acts=mk({|1 + ¦2|}) @ [Action.Select(Smart(2))],
    ~goal={|1 + §2¦|},
  ),
  test(
    ~name="Triple-click on infix operator: select full expression",
    ~acts=
      mk({|true ¦&& false|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§true && false¦|},
  ),
  test(
    ~name="Triple-click on single-char infix: select full expression",
    ~acts=
      mk({|1 ¦+ 2|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§1 + 2¦|},
  ),
  /* --- Tile edge Smart(2): select indicated child, not delimiter --- */
  test(
    ~name="Double-click at left paren edge: select indicated child",
    ~acts=mk({|(¦0 + 1, 1)|}) @ [Action.Select(Smart(2))],
    ~goal={|(§0¦ + 1, 1)|},
  ),
  test(
    ~name="Double-click at right paren edge: select indicated child",
    ~acts=mk({|(0 + 1, 1¦)|}) @ [Action.Select(Smart(2))],
    ~goal={|(0 + 1, §1¦)|},
  ),
  test(
    ~name="Double-click at left bracket edge: select indicated child",
    ~acts=mk({|[¦1, 2, 3]|}) @ [Action.Select(Smart(2))],
    ~goal={|[§1¦, 2, 3]|},
  ),
  test(
    ~name="Double-click at right bracket edge: select indicated child",
    ~acts=mk({|[1, 2, 3¦]|}) @ [Action.Select(Smart(2))],
    ~goal={|[1, 2, §3¦]|},
  ),
  /* --- Nested/compound terms --- */
  test(
    ~name="Double-click outside nested parens: select outer close paren",
    ~acts=mk({|((c))¦|}) @ [Action.Select(Smart(2))],
    ~goal={|((c)§)¦|},
  ),
  test(
    ~name="Triple-click outside nested parens: select outer term",
    ~acts=
      mk({|((c))¦|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§((c))¦|},
  ),
  test(
    ~name="Triple-click outside function application: select whole app",
    ~acts=
      mk({|f(n + 1)¦|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§f(n + 1)¦|},
  ),
  /* --- Function application inside tuple --- */
  test(
    ~name="Triple-click on first tuple element fn app: select fn app",
    ~acts=
      mk({|(¦odd(n), 1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(§odd(n)¦, 1)|},
  ),
  test(
    ~name="Triple-click on second tuple element fn app: select fn app",
    ~acts=
      mk({|(1, ¦odd(n))|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(1, §odd(n)¦)|},
  ),
  /* --- Smart(3) for non-term tokens (comma, operator inside parens) ---
   * These don't need statics because the indicated piece is not a term,
   * so Smart(3) uses current_term (term_data only) not parent_of_indicated. */
  test(
    ~name="Double-click on comma in parens: select comma",
    ~acts=mk({|(0 + 1¦, 1)|}) @ [Action.Select(Smart(2))],
    ~goal={|(0 + 1§,¦ 1)|},
  ),
  test(
    ~name="Triple-click on comma in parens: select paren expression",
    ~acts=
      mk({|(0 + 1¦, 1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§(0 + 1, 1)¦|},
  ),
  test(
    ~name="Triple-click on + inside parens: select plus expression",
    ~acts=
      mk({|(0 ¦+ 1, 1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(§0 + 1¦, 1)|},
  ),
  test(
    ~name="Double-click on comma in list: select comma",
    ~acts=mk({|[1¦, 2, 3]|}) @ [Action.Select(Smart(2))],
    ~goal={|[1§,¦ 2, 3]|},
  ),
  test(
    ~name="Triple-click on comma in list: select list expression",
    ~acts=
      mk({|[1¦, 2, 3]|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§[1, 2, 3]¦|},
  ),
  /* --- More Smart(2) tests: various syntactic forms --- */
  test(
    ~name="Double-click on identifier",
    ~acts=mk({|¦foo|}) @ [Action.Select(Smart(2))],
    ~goal={|§foo¦|},
  ),
  test(
    ~name="Double-click on integer literal",
    ~acts=mk({|¦42|}) @ [Action.Select(Smart(2))],
    ~goal={|§42¦|},
  ),
  test(
    ~name="Double-click on float literal",
    ~acts=mk({|¦3.14|}) @ [Action.Select(Smart(2))],
    ~goal={|§3.14¦|},
  ),
  test(
    ~name="Double-click on boolean",
    ~acts=mk({|¦true|}) @ [Action.Select(Smart(2))],
    ~goal={|§true¦|},
  ),
  test(
    ~name="Double-click on string literal",
    ~acts=mk({|¦"hello"|}) @ [Action.Select(Smart(2))],
    ~goal={|§"hello"¦|},
  ),
  test(
    ~name="Double-click on negation operator",
    ~acts=mk({|¦- 5|}) @ [Action.Select(Smart(2))],
    ~goal={|§-¦ 5|},
  ),
  test(
    ~name="Double-click on :: operator",
    ~acts=mk({|1 ¦:: []|}) @ [Action.Select(Smart(2))],
    ~goal={|1 §::¦ []|},
  ),
  test(
    ~name="Double-click on == operator",
    ~acts=mk({|x ¦== y|}) @ [Action.Select(Smart(2))],
    ~goal={|x §==¦ y|},
  ),
  test(
    ~name="Double-click on pipe operator",
    ~acts=mk({|x ¦|> f|}) @ [Action.Select(Smart(2))],
    ~goal={|x §|>¦ f|},
  ),
  /* --- Smart(2) edge cases with spacing --- */
  test(
    ~name="Double-click on infix without spaces: select operator",
    ~acts=mk({|1¦+2|}) @ [Action.Select(Smart(2))],
    ~goal={|1§+¦2|},
  ),
  test(
    ~name="Triple-click on infix without spaces: select expression",
    ~acts=
      mk({|1¦+2|}) @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§1+2¦|},
  ),
  /* --- More Smart(3) tests: escalation from term to parent --- */
  test(
    ~name="Triple-click on operand in binary op: select expression",
    ~acts=
      mk({|1 + ¦2|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§1 + 2¦|},
  ),
  test(
    ~name="Triple-click on left operand: select expression",
    ~acts=
      mk({|¦1 + 2|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§1 + 2¦|},
  ),
  test(
    ~name="Triple-click on boolean in &&: select expression",
    ~acts=
      mk({|¦true && false|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§true && false¦|},
  ),
  test(
    ~name="Triple-click on identifier in binary op: select expression",
    ~acts=
      mk({|¦x + y|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§x + y¦|},
  ),
  /* --- Smart(3) for let expressions --- */
  test(
    ~name="Triple-click on let keyword: select let tile only",
    ~acts=
      mk({|¦let x = 1 in x|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§let x = 1 in¦ x|},
  ),
  test(
    ~name="Triple-click on let body: select whole let expression",
    ~acts=
      mk({|let x = 1 in ¦x|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§let x = 1 in x¦|},
  ),
  /* --- Smart(3) for nested structures --- */
  test(
    ~name="Triple-click inside parens: select paren contents term",
    ~acts=
      mk({|(¦1 + 2)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(§1 + 2¦)|},
  ),
  test(
    ~name="Triple-click on inner term in nested parens: select inner paren",
    ~acts=
      mk({|((¦c))|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(§(c)¦)|},
  ),
  /* --- Smart(2) and Smart(3) for function application --- */
  test(
    ~name="Double-click on function name in app: select function name",
    ~acts=mk({|¦f(x)|}) @ [Action.Select(Smart(2))],
    ~goal={|§f¦(x)|},
  ),
  test(
    ~name="Triple-click on function name in app: select whole app",
    ~acts=
      mk({|¦f(x)|}) @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§f(x)¦|},
  ),
  test(
    ~name="Double-click on argument inside app parens: select argument",
    ~acts=mk({|f(¦x)|}) @ [Action.Select(Smart(2))],
    ~goal={|f(§x¦)|},
  ),
  test(
    ~name="Triple-click on argument inside app parens: select whole app",
    ~acts=
      mk({|f(¦x)|}) @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§f(x)¦|},
  ),
  /* --- App at designated position (between fn name and app parens) --- */
  test(
    ~name="Double-click between fn and app parens: select open paren token",
    ~acts=mk({|f¦(x)|}) @ [Action.Select(Smart(2))],
    ~goal={|f§(¦x)|},
  ),
  test(
    ~name="Triple-click between fn and app parens: select whole app",
    ~acts=
      mk({|f¦(x)|}) @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§f(x)¦|},
  ),
  test(
    ~name="Double-click between fn and app parens in tuple: select open paren",
    ~acts=mk({|(myfun¦(arg), 1)|}) @ [Action.Select(Smart(2))],
    ~goal={|(myfun§(¦arg), 1)|},
  ),
  test(
    ~name="Triple-click between fn and app parens in tuple: select app",
    ~acts=
      mk({|(myfun¦(arg), 1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|(§myfun(arg)¦, 1)|},
  ),
  /* --- Smart(2) for list and constructor --- */
  test(
    ~name="Double-click on element in list",
    ~acts=mk({|[1, ¦2, 3]|}) @ [Action.Select(Smart(2))],
    ~goal={|[1, §2¦, 3]|},
  ),
  test(
    ~name="Triple-click on element in list: select whole list",
    ~acts=
      mk({|[1, ¦2, 3]|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§[1, 2, 3]¦|},
  ),
  /* --- Smart(2) for constructor --- */
  test(
    ~name="Double-click on constructor name",
    ~acts=mk({|¦Some(1)|}) @ [Action.Select(Smart(2))],
    ~goal={|§Some¦(1)|},
  ),
  test(
    ~name="Triple-click on constructor name: select whole constructor app",
    ~acts=
      mk({|¦Some(1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§Some(1)¦|},
  ),
  /* --- Constructor at designated position --- */
  test(
    ~name="Double-click between constructor and app parens: select open paren",
    ~acts=mk({|Some¦(1)|}) @ [Action.Select(Smart(2))],
    ~goal={|Some§(¦1)|},
  ),
  test(
    ~name="Triple-click between constructor and app parens: select whole app",
    ~acts=
      mk({|Some¦(1)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
    ~goal={|§Some(1)¦|},
  ),
  /* --- Select(Term(Current)) with existing selection --- */
  /* When selection matches a term, Cmd+D escalates to parent */
  test(
    ~name="Cmd+D from no selection: select indicated term",
    ~acts=mk({|1 + ¦2|}) @ [Select(Term(Current))],
    ~goal={|1 + §2¦|},
  ),
  test(
    ~name="Cmd+D from term selection: escalate to parent",
    ~acts=
      mk({|1 + ¦2|}) @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§1 + 2¦|},
  ),
  test(
    ~name="Cmd+D on operand in tuple: escalate to binop",
    ~acts=
      mk({|(¦1 + 2, 3)|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|(§1 + 2¦, 3)|},
  ),
  test(
    ~name="Cmd+D on operand in tuple: escalate to tuple/parens",
    ~acts=
      mk({|(¦1 + 2, 3)|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§(1 + 2, 3)¦|},
  ),
  test(
    ~name="Cmd+D from Smart(2) token: round up to containing term",
    ~acts=mk({|1 ¦+ 2|}) @ [Select(Smart(2)), Select(Term(Current))],
    ~goal={|§1 + 2¦|},
  ),
  test(
    ~name="Cmd+D on function name: select fn, then escalate to app",
    ~acts=
      mk({|¦f(x)|}) @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§f(x)¦|},
  ),
  test(
    ~name="Cmd+D on arg inside app: select arg, then app",
    ~acts=
      mk({|f(¦x)|}) @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§f(x)¦|},
  ),
  test(
    ~name="Cmd+D on inner parens: escalate from inner to outer",
    ~acts=
      mk({|((¦c))|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§((c))¦|},
  ),
  test(
    ~name="Cmd+D on let body: select body",
    ~acts=mk({|let x = 1 in ¦x|}) @ [Select(Term(Current))],
    ~goal={|let x = 1 in §x¦|},
  ),
  test(
    ~name="Cmd+D on let body: body then full let",
    ~acts=
      mk({|let x = 1 in ¦x|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§let x = 1 in x¦|},
  ),
  test(
    ~name="Cmd+D on nested let body: select body",
    ~acts=mk({|let x = 1 in let y = 2 in ¦y|}) @ [Select(Term(Current))],
    ~goal={|let x = 1 in let y = 2 in §y¦|},
  ),
  test(
    ~name="Cmd+D on nested let body: body then inner let",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦y|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|let x = 1 in §let y = 2 in y¦|},
  ),
  test(
    ~name="Cmd+D on nested let body: body through outer let",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦y|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = 1 in let y = 2 in y¦|},
  ),
  test(
    ~name="Cmd+D on def header then full let: no cycling",
    ~acts=
      mk({|¦let x = 1 in x|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§let x = 1 in x¦|},
  ),
  test(
    ~name="Cmd+D inside tuple element: select element, then parens",
    ~acts=
      mk({|(1, ¦2, 3)|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§(1, 2, 3)¦|},
  ),
  test(
    ~name="Cmd+D in case rule body: select body",
    ~acts=mk({|case x | A => ¦1 | B => 2 end|}) @ [Select(Term(Current))],
    ~goal={|case x | A => §1¦ | B => 2 end|},
  ),
  test(
    ~name="Cmd+D in case rule body: body then rule",
    ~acts=
      mk({|case x | A => ¦1 | B => 2 end|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|case x §| A => 1¦ | B => 2 end|},
  ),
  test(
    ~name="Cmd+D in case rule body: body then rule then case",
    ~acts=
      mk({|case x | A => ¦1 | B => 2 end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§case x | A => 1 | B => 2 end¦|},
  ),
  test(
    ~name="Cmd+D nested case: body then inner rule",
    ~acts=
      mk({|case x | A => case y | C => ¦1 | D => 2 end | B => 3 end|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|case x | A => case y §| C => 1¦ | D => 2 end | B => 3 end|},
  ),
  test(
    ~name="Cmd+D nested case: inner rule then inner case",
    ~acts=
      mk({|case x | A => case y | C => ¦1 | D => 2 end | B => 3 end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|case x | A => §case y | C => 1 | D => 2 end¦ | B => 3 end|},
  ),
  test(
    ~name="Cmd+D nested case: inner case then outer rule",
    ~acts=
      mk({|case x | A => case y | C => ¦1 | D => 2 end | B => 3 end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|case x §| A => case y | C => 1 | D => 2 end¦ | B => 3 end|},
  ),
  test(
    ~name="Cmd+D nested case in last rule: step 1 cursor to inner case",
    ~acts=
      mk(
        {|case x | A => 1 | B => 2 | C => ¦case y | D => 3 | E => 4 end end|},
      )
      @ [Select(Term(Current))],
    ~goal=
      {|case x | A => 1 | B => 2 | C => §case y | D => 3 | E => 4 end¦ end|},
  ),
  test(
    ~name="Cmd+D nested case in last rule: step 2 inner case to outer rule",
    ~acts=
      mk(
        {|case x | A => 1 | B => 2 | C => ¦case y | D => 3 | E => 4 end end|},
      )
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal=
      {|case x | A => 1 | B => 2 §| C => case y | D => 3 | E => 4 end¦ end|},
  ),
  test(
    ~name="Cmd+D nested let compound body: step 1 cursor to x",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦x + y|}) @ [Select(Term(Current))],
    ~goal={|let x = 1 in let y = 2 in §x¦ + y|},
  ),
  test(
    ~name="Cmd+D nested let compound body: step 2 x to x+y",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦x + y|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|let x = 1 in let y = 2 in §x + y¦|},
  ),
  test(
    ~name="Cmd+D nested let compound body: step 3 x+y to inner let",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦x + y|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|let x = 1 in §let y = 2 in x + y¦|},
  ),
  test(
    ~name="Cmd+D nested let compound body: step 4 inner let to outer let",
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦x + y|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = 1 in let y = 2 in x + y¦|},
  ),
  test(
    ~name="Cmd+D nested case in last rule: step 3 outer rule to outer case",
    ~acts=
      mk(
        {|case x | A => 1 | B => 2 | C => ¦case y | D => 3 | E => 4 end end|},
      )
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal=
      {|§case x | A => 1 | B => 2 | C => case y | D => 3 | E => 4 end end¦|},
  ),
  /* --- Single let with compound body --- */
  test(
    ~name="Cmd+D single let compound body: x to x+y",
    ~acts=
      mk({|let x = 1 in ¦x + y|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|let x = 1 in §x + y¦|},
  ),
  test(
    ~name="Cmd+D single let compound body: x+y to let",
    ~acts=
      mk({|let x = 1 in ¦x + y|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = 1 in x + y¦|},
  ),
  /* --- Fun with compound body --- */
  test(
    ~name="Cmd+D fun compound body: x to x+y",
    ~acts=
      mk({|fun a -> ¦x + y|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|fun a -> §x + y¦|},
  ),
  test(
    ~name="Cmd+D fun compound body: x+y to fun",
    ~acts=
      mk({|fun a -> ¦x + y|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§fun a -> x + y¦|},
  ),
  /* --- If/then/else --- */
  test(
    ~name="Cmd+D in if condition: select cond then if",
    ~acts=
      mk({|if ¦x then y else z|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§if x then y else z¦|},
  ),
  test(
    ~name="Cmd+D in then branch: select body then if",
    ~acts=
      mk({|if x then ¦y else z|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§if x then y else z¦|},
  ),
  test(
    ~name="Cmd+D in else branch: select body then if",
    ~acts=
      mk({|if x then y else ¦z|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§if x then y else z¦|},
  ),
  /* --- Let inside case rule --- */
  test(
    ~name="Cmd+D let in case rule: body to let",
    ~acts=
      mk({|case x | A => let y = 1 in ¦y | B => 2 end|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|case x | A => §let y = 1 in y¦ | B => 2 end|},
  ),
  test(
    ~name="Cmd+D let in case rule: let to rule",
    ~acts=
      mk({|case x | A => let y = 1 in ¦y | B => 2 end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|case x §| A => let y = 1 in y¦ | B => 2 end|},
  ),
  test(
    ~name="Cmd+D let in case rule: rule to case",
    ~acts=
      mk({|case x | A => let y = 1 in ¦y | B => 2 end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§case x | A => let y = 1 in y | B => 2 end¦|},
  ),
  /* --- Case with single rule --- */
  test(
    ~name="Cmd+D in single-rule case: body to rule",
    ~acts=
      mk({|case x | A => ¦y end|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|case x §| A => y¦ end|},
  ),
  test(
    ~name="Cmd+D in single-rule case: rule to case",
    ~acts=
      mk({|case x | A => ¦y end|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§case x | A => y end¦|},
  ),
  /* --- Compound def expression --- */
  test(
    ~name="Cmd+D compound def: f to f(y)",
    ~acts=
      mk({|let x = ¦f(y) in x|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|let x = §f(y)¦ in x|},
  ),
  test(
    ~name="Cmd+D compound def: f(y) to let header",
    ~acts=
      mk({|let x = ¦f(y) in x|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = f(y) in¦ x|},
  ),
  test(
    ~name="Cmd+D compound def: let header to full let",
    ~acts=
      mk({|let x = ¦f(y) in x|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = f(y) in x¦|},
  ),
  /* --- Pipeline / chained binops --- */
  test(
    ~name="Cmd+D in chained binop: x to x+y (left assoc)",
    ~acts=
      mk({|¦x + y + z|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§x + y¦ + z|},
  ),
  test(
    ~name="Cmd+D in chained binop: x+y to x+y+z",
    ~acts=
      mk({|¦x + y + z|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§x + y + z¦|},
  ),
];

/* Check that no incomplete tiles exist anywhere in a segment (recursive). */
let rec seg_has_incomplete = (seg: Segment.t): bool =>
  List.exists(
    fun
    | Piece.Tile(t) =>
      !Tile.is_complete(t) || List.exists(seg_has_incomplete, t.children)
    | _ => false,
    seg,
  );

let zip_has_incomplete = (z: Zipper.t): bool =>
  seg_has_incomplete(Zipper.zip(z));

/* Test helper that checks printer output AND absence of incomplete tiles. */
let test_complete = (~name, ~acts, ~goal): test_case(_) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = acts |> perform(Zipper.init());
      let printed = printer(z);
      check(
        testable(Fmt.string, String.equal),
        "printer output",
        goal,
        printed,
      );
      if (zip_has_incomplete(z)) {
        Alcotest.fail("Incomplete tiles remain after rescan");
      };
    },
  );

let rescan_tests = [
  /* PHASE 1: Basic sibling-level rescan.
   * These test out-of-order typing where the rescan retroactively
   * matches standalone monotiles with incomplete tiles' missing shards.
   * Each test fails on dev (incomplete tiles remain) but passes with rescan. */
  /* fun/-> : type `a -> x`, go to start, type `fun ` */
  test_complete(
    ~name="Rescan: fun before standalone ->",
    ~acts=mk({|¦a -> x|}) @ string_to_ltr_actions("fun "),
    ~goal={|fun ¦a -> x|},
  ),
  /* let/=/in : type `= 1 in x`, go to start, type `let y ` */
  test_complete(
    ~name="Rescan: let before standalone = and in",
    ~acts=mk({|¦= 1 in x|}) @ string_to_ltr_actions("let y "),
    ~goal={|let y ¦= 1 in x|},
  ),
  /* if/then/else : type `a then b else c`, go to start, type `if ` */
  test_complete(
    ~name="Rescan: if before standalone then and else",
    ~acts=mk({|¦a then b else c|}) @ string_to_ltr_actions("if "),
    ~goal={|if ¦a then b else c|},
  ),
  /* Baseline: left-to-right typing should also have no incomplete tiles */
  test_complete(
    ~name="Baseline: fun a -> x (left-to-right)",
    ~acts=mk({|fun a -> x¦|}),
    ~goal={|fun a -> x¦|},
  ),
  test_complete(
    ~name="Baseline: let y = 1 in x (left-to-right)",
    ~acts=mk({|let y = 1 in x¦|}),
    ~goal={|let y = 1 in x¦|},
  ),
  /* Out-of-order delimiter tests: these type delimiters in non-label order.
   * Rescan should NOT match a delimiter whose shard index is lower than
   * an already-matched shard to its left (monotonicity constraint). */
  /* if/then/else : type `if 1 else 2 then` (then after else) */
  test(
    ~name="Rescan: if with then typed after else",
    ~acts=string_to_ltr_actions("if 1 else 2 then"),
    ~goal={|if 1 else 2 then¦?|},
  ),
  /* let/=/in : type `let x in 1 = 2` (= after in) */
  test(
    ~name="Rescan: let with = typed after in",
    ~acts=string_to_ltr_actions("let x in 1 = 2"),
    ~goal={|let x in 1 = 2¦|},
  ),
  /* type/=/in : type `type x in y = z` (= after in) */
  test(
    ~name="Rescan: type with = typed after in",
    ~acts=string_to_ltr_actions("type x in y = z"),
    ~goal={|type x in y = z¦|},
  ),
  /* PHASE 2: Effective-label matching (cross-form re-association).
   * These test that orphaned shards from one form can be matched by
   * a different form with compatible delimiters.
   * These should FAIL until effective-label matching is implemented. */
  /* Delete fun from `fun a -> x`, retype as fix.
   * The orphaned ->[1] (label ["fun","->"]) should match fix's ->. */
  test_complete(
    ~name="Effective label: fix reuses orphaned fun arrow",
    ~acts=
      mk({|¦fun a -> x|})
      @ [Destruct(Right), Destruct(Right), Destruct(Right)]
      @ string_to_ltr_actions("fix"),
    ~goal={|fix¦ a -> x|},
  ),
  /* Delete let from `let y = 1 in x`, retype as type.
   * The orphaned =[1] and in[2] should match type's = and in. */
  test_complete(
    ~name="Effective label: type reuses orphaned let = and in",
    ~acts=
      mk({|¦let y = 1 in x|})
      @ [Destruct(Right), Destruct(Right), Destruct(Right)]
      @ string_to_ltr_actions("type"),
    ~goal={|type¦ y = 1 in x|},
  ),
  /* Recovery workflow: fun (a, b -> ), insert ) after b, delete old ).
   * After inserting ), the new ) matches ( (backpack). Move to old ),
   * delete it. The rescan should then match fun with ->. */
  test_complete(
    ~name="Recovery: fun (a, b -> a) via insert-then-delete",
    ~acts=
      mk({|fun (a, b -> a)¦|})
      @ mv_l(6)  /* fun (a, b¦ -> ) */
      @ string_to_ltr_actions(")")  /* fun (a, b)¦ -> ) */
      @ mv_r(6)  /* fun (a, b) -> )¦ */
      @ [Destruct(Left)], /* delete old ) */
    ~goal={|fun (a, b) -> a¦|},
  ),
];

/* ===== PASTE CORRECTNESS TESTS =====
   These test paste behavior in various contexts. Tests marked [VALIDATED]
   produce WRONG results when the fast paste optimization is forced (guards
   bypassed). Tests marked [BASELINE] are correctness checks that happen to
   pass under both paths; their guards are either redundant with other guards
   or their edge case doesn't manifest in text output differences. */
let paste_tests = [
  /* TOKEN MERGING: LEFT BOUNDARY [VALIDATED]
     Clipboard first char merges with left neighbor token.
     Fast paste would parse clipboard in isolation, producing separate
     tokens with concave grout between them instead of merging. */
  test(
    ~name="Paste merging with left neighbor token",
    ~acts=mk("foo¦") @ [Paste("bar")],
    ~goal={|foobar¦|},
  ),
  test(
    ~name="Paste single char merging left into number",
    ~acts=mk("12¦") @ [Paste("3")],
    ~goal={|123¦|},
  ),
  /* TOKEN MERGING: RIGHT BOUNDARY [VALIDATED]
     Clipboard last char merges with right neighbor token.
     Fast paste keeps them separate with grout. Slow path merges. */
  test(
    ~name="Paste merging with right neighbor token",
    ~acts=mk("¦bar") @ [Paste("foo")],
    ~goal={|foo¦bar|},
  ),
  test(
    ~name="Paste number merging with right neighbor",
    ~acts=mk("¦23") @ [Paste("1")],
    ~goal={|1¦23|},
  ),
  /* INNER CARET (inside a token) [VALIDATED]
     Caret is between characters of a token. Fast paste can't split
     a token and produces a completely wrong structure. */
  test(
    ~name="Paste at inner caret position",
    ~acts=mk("fo¦o") @ [Paste("x")],
    ~goal={|fox¦o|},
  ),
  test(
    ~name="Paste multiple chars at inner caret",
    ~acts=mk("he¦o") @ [Paste("ll")],
    ~goal={|hell¦o|},
  ),
  /* INSIDE NESTED STRUCTURE (ancestors non-empty) [VALIDATED]
     Caret is inside parens/brackets. Fast paste splices at the wrong
     level, placing content outside the delimiters instead of inside. */
  test(
    ~name="Paste expression inside parens",
    ~acts=mk("(¦)") @ [Paste("1 + 2")],
    ~goal={|(1 + 2¦)|},
  ),
  test(
    ~name="Paste inside nested parens",
    ~acts=mk("((¦))") @ [Paste("42")],
    ~goal={|((42¦))|},
  ),
  test(
    ~name="Paste inside list literal",
    ~acts=mk("[¦]") @ [Paste("1, 2, 3")],
    ~goal={|[1, 2, 3¦]|},
  ),
  /* NON-EMPTY BACKPACK [BASELINE]
     Insert "(" puts caret inside parens (ancestors non-empty), so
     fast_paste returns None via ancestors check before backpack check.
     This tests correctness of paste inside an incomplete form. */
  test(
    ~name="Paste with pending close paren in backpack",
    ~acts=mk("¦") @ [Insert("(")] @ [Paste("1 + 2")],
    ~goal={|(1 + 2¦|},
  ),
  /* NON-EXP SORT [BASELINE]
     At top level ancestors==[] implies sort==Exp, so the sort guard
     is redundant with the ancestors guard. Inside a type ascription,
     ancestors!=[] catches it first. This tests paste correctness in
     type position regardless. */
  test(
    ~name="Paste type arrow in type annotation position",
    ~acts=mk("1 : ¦") @ [Paste("Int -> Int")],
    ~goal={|1 : Int -> Int¦|},
  ),
  /* UNBALANCED DELIMITERS IN CLIPBOARD [VALIDATED]
     Clipboard with unmatched parens/brackets. Fast paste would lose
     the unmatched delimiters (they end up in parsing backpack, which
     is discarded during segment extraction). Slow path preserves
     them in the target zipper's backpack. */
  test(
    ~name="Paste unbalanced parens (backpack glom)",
    ~acts=mk("¦") @ [Paste({|([)(|})],
    ~goal={|([?)(¦?|},
  ),
  test(
    ~name="Paste unmatched open paren",
    ~acts=mk("¦") @ [Paste("(1 + 2")],
    ~goal={|(1 + 2¦|},
  ),
  /* NORMAL TOP-LEVEL PASTE (fast path eligible)
     These work correctly regardless of which path is taken.
     They serve as baseline correctness checks. */
  test(
    ~name="Paste simple expression at top level",
    ~acts=mk("¦") @ [Paste("1 + 2")],
    ~goal={|1 + 2¦|},
  ),
  test(
    ~name="Paste let binding at top level",
    ~acts=mk("¦") @ [Paste("let x = 1 in x")],
    ~goal={|let x = 1 in x¦|},
  ),
  test(
    ~name="Paste after complete expression with space separator",
    ~acts=mk("1 + 2 ¦") @ [Paste("+ 3")],
    ~goal={|1 + 2 + 3¦|},
  ),
  /* MULTI-LINE PASTE */
  test(
    ~name="Paste multi-line let bindings",
    ~acts=mk("¦") @ [Paste("let x = 1 in\nlet y = 2 in\nx + y")],
    ~goal={|let x = 1 in
let y = 2 in
x + y¦|},
  ),
];

/* ===== MODULE EDITING TESTS =====
   NOTE: These test basic module syntax editing behavior.
   `{` is an instant-expanding delimiter that creates `{¦}`.
   Inside braces is Mod sort, where `let` creates ModLet forms. */
let module_tests = [
  /* { is an instant expander: typing { puts } in the backpack.
     The printer shows backpack contents as missing, so } doesn't
     appear until Put_down. The ? inside is the empty Mod hole. */
  test(
    ~name="Module: Insert open brace (} in backpack)",
    ~acts=mk({|¦|}) @ [Insert("{")],
    ~goal={|{¦?|},
  ),
  test(
    ~name="Module: Complete empty module with Put_down",
    ~acts=mk({|¦|}) @ [Insert("{"), Put_down],
    ~goal={|{?}¦|},
  ),
  test(
    ~name="Module: Type let inside module",
    ~acts=
      mk({|¦|})
      @ [Insert("{")]
      @ string_to_ltr_actions(" let x = 1 ")
      @ [Put_down],
    ~goal={|{ let x = 1 }¦|},
  ),
  test(
    ~name="Module: Empty module as let definition",
    ~acts=mk({|let m = ¦|}) @ [Insert("{"), Put_down],
    ~goal={|let m = {?}¦|},
  ),
  /* --- Module Cmd+D selection tests --- */
  test(
    ~name="Module Cmd+D: step 1 value to ModLet",
    ~acts=
      mk({|let m = { let x = ¦1; let y = 2 } in m|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|let m = { §let x = 1¦; let y = 2 } in m|},
  ),
  test(
    ~name="Module Cmd+D: step 2 ModLet to module",
    ~acts=
      mk({|let m = { let x = ¦1; let y = 2 } in m|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|let m = §{ let x = 1; let y = 2 }¦ in m|},
  ),
  test(
    ~name="Module Cmd+D: second ModLet to module",
    ~acts=
      mk({|let m = { let x = 1; let y = ¦2 } in m|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|let m = §{ let x = 1; let y = 2 }¦ in m|},
  ),
  test(
    ~name="Module Cmd+D: ModType to module",
    ~acts=
      mk({|let m = { type T = ¦Int; let x = 1 } in m|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|let m = §{ type T = Int; let x = 1 }¦ in m|},
  ),
  /* --- Module with 3 items: shouldn't jump between items --- */
  test(
    ~name="Module Cmd+D: last of 3 items to module",
    ~acts=
      mk({|let m = { let a = 1; let b = 2; let c = ¦3 } in m|})
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|let m = §{ let a = 1; let b = 2; let c = 3 }¦ in m|},
  ),
];

/* ===== SHARD THEFT / PREPEND EDITING TESTS =====
   These test scenarios where typing new code directly before existing
   multi-delimiter forms (let/=/in, fun/->, if/then/else) causes delimiter
   mis-association. The core issue: appending a character to an adjacent
   shard of a complete tile disassembles it, and rescan greedily steals
   the orphaned shards for the nearest incomplete tile.

   The test_complete helper checks both printer output AND that no
   incomplete tiles remain, which catches the structural breakage. */

/* Helper: check that a zipper has a non-empty backpack (i.e. there are
 * missing shards from incomplete tiles visible at the caret position).
 * A non-empty backpack after entering what should be a complete program
 * indicates structural breakage. */
let zip_backpack_empty = (z: Zipper.t): bool =>
  Zipper.local_backpack(z) == [];

let shard_theft_tests = [
  /* Baseline: typing `let y = 2 in let x = 1 in x` left-to-right
   * should produce a complete program with no incomplete tiles. */
  test_complete(
    ~name="Baseline: nested let (left-to-right)",
    ~acts=mk({|let y = 2 in let x = 1 in x¦|}),
    ~goal={|let y = 2 in let x = 1 in x¦|},
  ),
  /* Core bug: type `let x = 1 in x`, move to start, type `let y = 2 in `.
   * This should produce the same complete program as the baseline.
   * Currently FAILS: the first `let` steals `=` and `in` from the
   * original `let x = 1 in x` when `y` merges with the original `let`
   * shard, and sort-specific expansion prevents recovery. */
  test_complete(
    ~name="Prepend let definition before existing let",
    ~acts=mk({|¦let x = 1 in x|}) @ string_to_ltr_actions("let y = 2 in "),
    ~goal={|let y = 2 in ¦let x = 1 in x|},
  ),
  /* Diagnostic: trace the zipper state after each character of "let y"
   * typed before an existing `let x = 1 in x`. */
  test_case(
    "Prepend let y: trace intermediate states",
    `Quick,
    () => {
      let init_acts = mk({|¦let x = 1 in x|});
      let z0 = init_acts |> perform(Zipper.init());
      let piece_summary = (p: Piece.t): string =>
        switch (p) {
        | Tile(t) =>
          let eff = Tile.effective_label(t);
          let sstr =
            t.shards |> List.map(string_of_int) |> String.concat(",");
          Printf.sprintf(
            "T(%s shards=[%s]%s)",
            String.concat(" ", eff),
            sstr,
            Tile.is_complete(t) ? "" : " INCOMPLETE",
          );
        | Secondary(s) =>
          Printf.sprintf("S(%s)", Secondary.get_string(s.content))
        | Grout(g) =>
          Printf.sprintf(
            "G(%s)",
            switch (g.shape) {
            | Convex => "convex"
            | Concave => "concave"
            },
          )
        | Projector(_) => "Proj"
        };
      let (l0, r0) = z0.relatives.siblings;
      Printf.printf(
        "INIT left=[%s] right=[%s]\n",
        l0 |> List.map(piece_summary) |> String.concat(", "),
        r0 |> List.map(piece_summary) |> String.concat(", "),
      );
      let (lt0, rt0) = Zipper.neighbor_tokens(z0);
      Printf.printf(
        "INIT neighbor_tokens: left=%s right=%s\n",
        switch (lt0) {
        | None => "None"
        | Some(t) => Printf.sprintf("Some(%s)", t)
        },
        switch (rt0) {
        | None => "None"
        | Some(t) => Printf.sprintf("Some(%s)", t)
        },
      );
      let chars = Token.to_list("let y = 2 in ");
      let _ =
        List.fold_left(
          (z, c) => {
            let z' = perform(z, [Action.Insert(c)]);
            let text = printer(z');
            let bp = Zipper.local_backpack(z');
            let bp_labels =
              bp
              |> List.map((t: Tile.t) => String.concat(",", t.label))
              |> String.concat("; ");
            let global_seg = Relatives.zip(z'.relatives);
            let global_bp =
              Segment.global_missing_shards(global_seg)
              |> List.map((t: Tile.t) => String.concat(",", t.label))
              |> String.concat("; ");
            let anc_info =
              switch (z'.relatives.ancestors) {
              | [] => "no ancestor"
              | [(a, _), ..._] =>
                let label = String.concat(",", a.label);
                let (sl, sr) = a.shards;
                let shards =
                  sl @ sr |> List.map(string_of_int) |> String.concat(",");
                Printf.sprintf("ancestor: %s shards=[%s]", label, shards);
              };
            let (ls, rs) = z'.relatives.siblings;
            let l_summary =
              ls |> List.map(piece_summary) |> String.concat(", ");
            let r_summary =
              rs |> List.map(piece_summary) |> String.concat(", ");
            Printf.printf(
              "After '%s': %s | local=[%s] | global=[%s] | %s\n  L=[%s]\n  R=[%s]\n",
              c,
              text,
              bp_labels,
              global_bp,
              anc_info,
              l_summary,
              r_summary,
            );
            z';
          },
          z0,
          chars,
        );
      let z_final =
        mk({|¦let x = 1 in x|})
        @ string_to_ltr_actions("let y = 2 in ")
        |> perform(Zipper.init());
      let has_incomplete = zip_has_incomplete(z_final);
      Printf.printf(
        "FINAL: %s | has_incomplete=%b\n",
        printer(z_final),
        has_incomplete,
      );
      if (has_incomplete) {
        Alcotest.fail(
          "Incomplete tiles remain after prepending let y = 2 in",
        );
      };
    },
  ),
  /* Similar bug with fun/->: type `fun x -> e`, prepend `fun y -> `. */
  test_complete(
    ~name="Prepend fun definition before existing fun",
    ~acts=mk({|¦fun x -> e|}) @ string_to_ltr_actions("fun y -> "),
    ~goal={|fun y -> ¦fun x -> e|},
  ),
  /* Similar bug with if/then/else */
  test_complete(
    ~name="Prepend if before existing if",
    ~acts=
      mk({|¦if a then b else c|})
      @ string_to_ltr_actions("if d then e else "),
    ~goal={|if d then e else ¦if a then b else c|},
  ),
  /* Baseline: fresh-typed nested fun should be complete */
  test_complete(
    ~name="Baseline: nested fun (left-to-right)",
    ~acts=mk({|fun y -> fun x -> e¦|}),
    ~goal={|fun y -> fun x -> e¦|},
  ),
  /* Prepend type before existing type */
  test_complete(
    ~name="Prepend type before existing type",
    ~acts=
      mk({|¦type T = Int in T|})
      @ string_to_ltr_actions("type S = Bool in "),
    ~goal={|type S = Bool in ¦type T = Int in T|},
  ),
  /* Delete leading char of keyword, retype it.
   * After deletion, tile decomposes (et is a monotile, not multi-shard),
   * so the guard doesn't interfere. Retyping `l` creates a new let form
   * that picks up orphaned = and in shards. Result has no incomplete tiles. */
  test_complete(
    ~name="Delete and retype leading char of let keyword",
    ~acts=
      mk({|let x = 1 in x¦|})
      @ mv_l(14)
      @ [Destruct(Right)]
      @ [Insert("l")],
    ~goal={|l¦et x = 1 in x|},
  ),
  /* Type identifier before complete let — guard blocks merge,
   * identifier stays separate. Grout (~) inserted between y and let. */
  test_complete(
    ~name="Type identifier before complete let",
    ~acts=mk({|¦let x = 1 in x|}) @ string_to_ltr_actions("y "),
    ~goal={|y ¦~let x = 1 in x|},
  ),
];

/* Segment paste cache tests.
 * These test the internal optimization where copy/paste reuses the
 * parsed segment tree instead of re-parsing from text. If this
 * optimization is removed, these tests can be deleted.
 *
 * We test try_segment_paste directly since the cache is populated
 * at the UI layer (Page.re) which isn't available in unit tests. */

let segment_cache_test =
    (~name, ~setup: string, ~cache_text: string, ~paste_text: string, ~expect)
    : test_case(_) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = perform(Zipper.init(), mk(setup));
      /* Populate the segment cache */
      let seg = Parser.to_segment(cache_text);
      Parser.set_segment_cache(seg, cache_text);
      let result = Parser.try_segment_paste(paste_text, z);
      let got =
        switch (result) {
        | Some(_) => `Hit
        | None => `Miss
        };
      check(
        testable(Fmt.string, String.equal),
        name,
        switch (expect) {
        | `Hit => "Hit"
        | `Miss => "Miss"
        },
        switch (got) {
        | `Hit => "Hit"
        | `Miss => "Miss"
        },
      );
    },
  );

let segment_cache_tests = [
  segment_cache_test(
    ~name="Cache hit: paste matching text at token boundary",
    ~setup="1 + ¦",
    ~cache_text="2 + 3",
    ~paste_text="2 + 3",
    ~expect=`Hit,
  ),
  segment_cache_test(
    ~name="Cache miss: left token merge (foo + bar = foobar)",
    ~setup="foo¦",
    ~cache_text="bar",
    ~paste_text="bar",
    ~expect=`Miss,
  ),
  segment_cache_test(
    ~name="Cache miss: right token merge (bar + foo = barfoo)",
    ~setup="¦foo",
    ~cache_text="bar",
    ~paste_text="bar",
    ~expect=`Miss,
  ),
  segment_cache_test(
    ~name="Cache miss: inner caret",
    ~setup="fo¦o",
    ~cache_text="bar",
    ~paste_text="bar",
    ~expect=`Miss,
  ),
  segment_cache_test(
    ~name="Cache miss: text doesn't match cache",
    ~setup="1 + ¦",
    ~cache_text="2 + 3",
    ~paste_text="something else",
    ~expect=`Miss,
  ),
  /* Verify the full paste pipeline produces correct results
   * when segment cache is populated (integration-style) */
  test(
    ~name="Paste with warm cache produces same result as cold paste",
    ~acts={
      let text = "2 + 3";
      let seg = Parser.to_segment(text);
      Parser.set_segment_cache(seg, text);
      mk("1 + ¦") @ [Paste(text)];
    },
    ~goal="1 + 2 + 3¦",
  ),
];

/* ===== REMOLD SORT TESTS =====
   These test that tile mold sorts remain correct after editing operations.
   The bug: editing inside a child segment (e.g., inserting a space inside
   parentheses in type position) can cause the parentheses to be remolded
   as Exp instead of Typ. */

/* Recursively find all tiles with a given label in a segment. */
let rec find_tiles_by_label =
        (label: list(string), seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    fun
    | Piece.Tile(t) => {
        let matches = t.label == label ? [t] : [];
        let child_matches =
          List.concat_map(find_tiles_by_label(label), t.children);
        matches @ child_matches;
      }
    | _ => [],
    seg,
  );

let remold_sort_tests = [
  /* BUG: Type `1:(Int)`, place caret inside parens at `1:(Int|)`,
   * press space. The parentheses should remain molded as Typ (they
   * are in the type position of an ascription), but they get
   * incorrectly remolded as Exp. */
  test_case(
    "Remold: space inside type parens preserves Typ sort",
    `Quick,
    () => {
      let z = mk({|1:(Int¦)|}) @ [Insert(" ")] |> perform(Zipper.init());
      let seg = Zipper.zip(z);
      let paren_tiles = find_tiles_by_label(["(", ")"], seg);
      switch (paren_tiles) {
      | [] => Alcotest.fail("No paren tiles found in segment")
      | _ =>
        List.iter(
          (t: Tile.t) =>
            if (t.mold.out != Sort.Typ) {
              Alcotest.fail(
                Printf.sprintf(
                  "Paren tile has mold.out=%s, expected Typ",
                  Sort.show(t.mold.out),
                ),
              );
            },
          paren_tiles,
        )
      };
    },
  ),
  /* Baseline: the same program without the space edit should have Typ parens. */
  test_case(
    "Baseline: type parens in 1:(Int) have Typ sort",
    `Quick,
    () => {
      let z = mk({|1:(Int¦)|}) |> perform(Zipper.init());
      let seg = Zipper.zip(z);
      let paren_tiles = find_tiles_by_label(["(", ")"], seg);
      switch (paren_tiles) {
      | [] => Alcotest.fail("No paren tiles found in segment")
      | _ =>
        List.iter(
          (t: Tile.t) =>
            if (t.mold.out != Sort.Typ) {
              Alcotest.fail(
                Printf.sprintf(
                  "Paren tile has mold.out=%s, expected Typ",
                  Sort.show(t.mold.out),
                ),
              );
            },
          paren_tiles,
        )
      };
    },
  ),
];

/* === Selection Wrapping Tests ===
 * Test wrapping selected content in delimiters by typing the
 * opening delimiter with an active selection. */
let wrap_selection_tests = [
  /* --- Balanced delimiter wrapping (parens, brackets, braces) --- */
  test(
    ~name="Wrap single token in parens",
    ~acts=
      mk({|¦x + y|})
      @ [Action.Select(Resize(Local(Right, ByToken)))]
      @ [Action.Insert("(")],
    ~goal={|(§x¦) + y|},
  ),
  test(
    ~name="Wrap expression in parens via Select(All)",
    ~acts=mk({|¦1 + 2|}) @ [Action.Select(All)] @ [Action.Insert("(")],
    ~goal={|(§1 + 2¦)|},
  ),
  test(
    ~name="Wrap expression in square brackets via Select(All)",
    ~acts=mk({|¦1 + 2|}) @ [Action.Select(All)] @ [Action.Insert("[")],
    ~goal={|[§1 + 2¦]|},
  ),
  test(
    ~name="Wrap expression in curly braces via Select(All)",
    ~acts=mk({|¦1 + 2|}) @ [Action.Select(All)] @ [Action.Insert("{")],
    ~goal={|{§1 + 2¦}|},
  ),
  test(
    ~name="Wrap single operand via Term(Current)",
    ~acts=
      mk({|¦1 + 2|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")],
    ~goal={|(§1¦) + 2|},
  ),
  test(
    ~name="Wrap subexpression in parens",
    ~acts=
      mk({|1 + ¦2 * 3|})
      @ [Action.Select(Resize(Local(Right, ByToken)))]
      @ [Action.Insert("(")],
    ~goal={|1 + (§2¦) * 3|},
  ),
  test(
    ~name="Wrap in parens then unselect and type after",
    ~acts=
      mk({|¦x|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")]
      @ [Action.Unselect(None)]
      @ string_to_ltr_actions(" + z"),
    ~goal={|(x + z¦)|},
  ),
  /* --- Quote wrapping (string, label, comment) --- */
  test(
    ~name="Wrap token in string quotes",
    ~acts=
      mk({|¦abc|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert({|"|})],
    ~goal={|"abc"¦|},
  ),
  test(
    ~name="Wrap token in backtick quotes",
    ~acts=
      mk({|¦abc|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("`")],
    ~goal={|`abc`¦|},
  ),
  test(
    ~name="Wrap token in comment delimiters",
    ~acts=
      mk({|¦abc|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("#")],
    ~goal={|?#abc#¦|},
  ),
  /* --- Quote wrapping validation (fallthrough to replacement) --- */
  test(
    ~name=
      "String wrap fails with embedded quote: falls through to replacement",
    ~acts=
      mk({|¦"hello"|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert({|"|})],
    ~goal={|"¦"|},
  ),
  /* --- Closing delimiter does NOT wrap (replaces selection) --- */
  test(
    ~name="Closing paren replaces selection, does not wrap",
    ~acts=
      mk({|¦x + y|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert(")")],
    ~goal={|?)¦ + y|},
  ),
  test(
    ~name="Closing bracket replaces selection, does not wrap",
    ~acts=
      mk({|¦x + y|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("]")],
    ~goal={|?]¦ + y|},
  ),
  /* --- Edge cases --- */
  test(
    ~name="Wrap empty hole in parens",
    ~acts=
      mk({|let x = ¦? in x|})
      @ [Action.Select(Resize(Local(Right, ByToken)))]
      @ [Action.Insert("(")],
    ~goal={|let x = (§?¦) in x|},
  ),
  test(
    ~name="Wrap parenthesized expression adds outer parens",
    ~acts=
      mk({|¦(1 + 2)|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")],
    ~goal={|(§(1 + 2)¦)|},
  ),
  test(
    ~name="Wrap multi-token selection in parens via char select",
    ~acts=
      mk({|¦x + y|})
      @ [Action.Select(Resize(Local(Right, ByChar)))]
      @ [Action.Select(Resize(Local(Right, ByChar)))]
      @ [Action.Select(Resize(Local(Right, ByChar)))]
      @ [Action.Select(Resize(Local(Right, ByChar)))]
      @ [Action.Select(Resize(Local(Right, ByChar)))]
      @ [Action.Insert("(")],
    ~goal={|(§x + y¦)|},
  ),
  test(
    ~name="Wrap single number in brackets",
    ~acts=
      mk({|¦42|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("[")],
    ~goal={|[§42¦]|},
  ),
  test(
    ~name="Double wrap: parens then brackets",
    ~acts=
      mk({|¦x|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")]
      @ [Action.Insert("[")],
    ~goal={|([§x¦])|},
  ),
  test(
    ~name="Wrap string literal in parens",
    ~acts=
      mk({|¦"hello"|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")],
    ~goal={|(§"hello"¦)|},
  ),
  test(
    ~name="Wrap in pattern context",
    ~acts=
      mk({|let ¦x = 1 in x|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("(")],
    ~goal={|let (§x¦) = 1 in x|},
  ),
  test(
    ~name="Backtick wrap fails on backtick content: falls through",
    ~acts=
      mk({|¦abc|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("`")]
      @ [Action.Select(Term(Current))]
      @ [Action.Insert("`")],
    ~goal={|`¦`|},
  ),
  test(
    ~name="Left-focused selection wraps correctly",
    ~acts=
      mk({|x + y¦|})
      @ [Action.Select(Resize(Local(Left, ByToken)))]
      @ [Action.Insert("(")],
    ~goal={|x + (§y¦)|},
  ),
];

let unwrap_quote_tests = [
  /* --- String unwrapping --- */
  test(
    ~name="Backspace string from right unwraps content",
    ~acts=mk({|"hello"¦|}) @ [Action.Destruct(Left)],
    ~goal={|hello¦|},
  ),
  test(
    ~name="Delete string from left unwraps content",
    ~acts=mk({|¦"hello"|}) @ [Action.Destruct(Right)],
    ~goal={|hello¦|},
  ),
  test(
    ~name="Backspace empty string just deletes",
    ~acts=mk({|""¦|}) @ [Action.Destruct(Left)],
    ~goal={|¦?|},
  ),
  test(
    ~name="Single char string unwraps",
    ~acts=mk({|"a"¦|}) @ [Action.Destruct(Left)],
    ~goal={|a¦|},
  ),
  /* --- Backtick label unwrapping --- */
  test(
    ~name="Backspace backtick label unwraps",
    ~acts=mk({|`abc`¦|}) @ [Action.Destruct(Left)],
    ~goal={|abc¦|},
  ),
  /* --- Comment unwrapping --- */
  test(
    ~name="Delete comment from left unwraps",
    ~acts=mk({|¦#stuff#|}) @ [Action.Destruct(Right)],
    ~goal={|stuff¦|},
  ),
  /* --- Context preservation --- */
  test(
    ~name="Unwrap string in expression context",
    ~acts=mk({|x + "hello"¦|}) @ [Action.Destruct(Left)],
    ~goal={|x + hello¦|},
  ),
  test(
    ~name="Unwrap string in let binding",
    ~acts=mk({|let x = "hello"¦ in x|}) @ [Action.Destruct(Left)],
    ~goal={|let x = hello¦ in x|},
  ),
  /* --- Content re-parses as code --- */
  test(
    ~name="Unwrap string with spaces produces separate tokens",
    ~acts=mk({|"hello world"¦|}) @ [Action.Destruct(Left)],
    ~goal={|hello ~world¦|},
  ),
  test(
    ~name="Unwrap string with operators re-parses as expression",
    ~acts=mk({|"1 + 2"¦|}) @ [Action.Destruct(Left)],
    ~goal={|1 + 2¦|},
  ),
  /* --- Inner boundary deletion --- */
  test(
    ~name="Backspace at opening delimiter boundary unwraps",
    ~acts=mk({|¦"hello"|}) @ mv_r(1) @ [Action.Destruct(Left)],
    ~goal={|hello¦|},
  ),
  test(
    ~name="Delete at closing delimiter boundary unwraps",
    ~acts=mk({|"hello"¦|}) @ mv_l(1) @ [Action.Destruct(Right)],
    ~goal={|hello¦|},
  ),
  /* --- Roundtrip with wrapping --- */
  test(
    ~name="Wrap then unwrap is identity",
    ~acts=
      mk({|¦abc|})
      @ [Action.Select(Term(Current))]
      @ [Action.Insert({|"|})]
      @ [Action.Destruct(Left)],
    ~goal={|abc¦|},
  ),
];

/* --- Comment Toggle tests --- */
let comment_toggle_tests = [
  /* Single line commenting */
  test(
    ~name="Comment a code line",
    ~acts=mk({|¦hello|}) @ [Action.ToggleLineComment],
    ~goal={|?#hello#¦|},
  ),
  test(
    ~name="Comment an expression",
    ~acts=mk({|¦1 + 2|}) @ [Action.ToggleLineComment],
    ~goal={|?#1 + 2#¦|},
  ),
  test(
    ~name="Comment with caret in middle of line",
    ~acts=mk({|he¦llo|}) @ [Action.ToggleLineComment],
    ~goal={|?#hello#¦|},
  ),
  /* Toggle empty line is a no-op */
  test(
    ~name="Toggle empty line is no-op",
    ~acts=mk({|¦|}) @ [Action.ToggleLineComment],
    ~goal={|?¦|},
  ),
  /* Roundtrip: comment then uncomment */
  test(
    ~name="Roundtrip: comment then uncomment identifier",
    ~acts=
      mk({|¦hello|})
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
    ~goal={|hello¦|},
  ),
  test(
    ~name="Roundtrip: comment then uncomment expression",
    ~acts=
      mk({|¦1 + 2|})
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
    ~goal={|1 + 2¦|},
  ),
  test(
    ~name="Roundtrip: comment then uncomment with caret in middle",
    ~acts=
      mk({|he¦llo|})
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
    ~goal={|hello¦|},
  ),
  /* Single line in multiline context */
  test(
    ~name="Comment second line only",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("y")
      @ [Action.ToggleLineComment],
    ~goal="x\n# y#¦",
  ),
  test(
    ~name="Comment second line roundtrip",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("y")
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
    ~goal="x\n ~y¦",
  ),
  /* Multi-line with selection: comment all lines */
  test(
    ~name="Multi: select all and comment two lines",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("y")
      @ [Action.Select(All)]
      @ [Action.ToggleLineComment],
    ~goal="§?#x#\n# y#¦",
  ),
  /* Multi-line: select one line and comment */
  test(
    ~name="Multi: select line 2 and comment",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("y")
      @ [Action.Select(Resize(Line(Left)))]
      @ [Action.ToggleLineComment],
    ~goal="x\n§# y#¦",
  ),
  /* Multi-line: mixed state does nothing */
  test(
    ~name="Multi: mixed code and comment is no-op",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("#y#")
      @ [Action.Select(All)]
      @ [Action.ToggleLineComment],
    ~goal="x\n#y#¦",
  ),
  /* Multi-line roundtrip */
  test(
    ~name="Multi: comment all then uncomment all",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n")
      @ string_to_ltr_actions("y")
      @ [Action.Select(All)]
      @ [Action.ToggleLineComment]
      @ [Action.Select(All)]
      @ [Action.ToggleLineComment],
    ~goal="§x\n ~y¦",
  ),
];

/* Collect (token, sort) pairs from all tiles in a segment, recursively */
let rec tile_sorts_of_seg = (seg: Segment.t): list((string, Sort.t)) =>
  List.concat_map(tile_sorts_of_piece, seg)
and tile_sorts_of_piece = (p: Piece.t): list((string, Sort.t)) =>
  switch (p) {
  | Tile(t) =>
    let label_sorts = List.map(tok => (tok, t.mold.out), t.label);
    let child_sorts = List.concat_map(tile_sorts_of_seg, t.children);
    label_sorts @ child_sorts;
  | Projector({syntax, _}) => tile_sorts_of_piece(syntax)
  | Grout(_)
  | Secondary(_) => []
  };

let tile_sorts_of_zip = (z: Zipper.t): list((string, Sort.t)) =>
  tile_sorts_of_seg(Zipper.zip(z));

let show_tile_sorts = (sorts: list((string, Sort.t))): string =>
  sorts
  |> List.map(((tok, sort)) => tok ++ ":" ++ Sort.show(sort))
  |> String.concat(" ");

/* Test that molds match between fresh typing and comment roundtrip */
let remold_test = (~name, ~fresh_acts, ~roundtrip_acts) =>
  test_case(
    name,
    `Quick,
    () => {
      let fresh_z = fresh_acts |> perform(Zipper.init());
      let roundtrip_z = roundtrip_acts |> perform(Zipper.init());
      let fresh_str = show_tile_sorts(tile_sorts_of_zip(fresh_z));
      let roundtrip_str = show_tile_sorts(tile_sorts_of_zip(roundtrip_z));
      check(
        testable(Fmt.string, String.equal),
        name,
        fresh_str,
        roundtrip_str,
      );
    },
  );

let comment_remold_tests = [
  /* Single-line let roundtrip */
  remold_test(
    ~name="Molds: single-line let roundtrip",
    ~fresh_acts=mk({|¦let a = 1 in a|}),
    ~roundtrip_acts=
      mk({|¦let a = 1 in a|})
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
  ),
  /* Multi-line let: comment/uncomment first line */
  remold_test(
    ~name="Molds: multi-line let roundtrip line 1",
    ~fresh_acts=mk({|let a =
1
in a¦|}),
    ~roundtrip_acts=
      mk({|let a =
1
in a¦|})
      @ [Action.Move(Start)]
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
  ),
  /* Annotated let roundtrip */
  remold_test(
    ~name="Molds: annotated let roundtrip",
    ~fresh_acts=mk({|¦let a : (Int) = 1 in a|}),
    ~roundtrip_acts=
      mk({|¦let a : (Int) = 1 in a|})
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
  ),
];

/* Additional comment toggle coverage */
let comment_toggle_extra_tests = [
  /* Uncomment a manually-typed comment */
  test(
    ~name="Uncomment manually typed comment",
    ~acts=mk({|¦#hello#|}) @ [Action.ToggleLineComment],
    ~goal={|hello¦|},
  ),
  /* Mixed single line (code + comment) is no-op */
  test(
    ~name="Mixed single line is no-op",
    ~acts=mk({|x ¦#hello#|}) @ [Action.ToggleLineComment],
    ~goal={|x #hello#¦|},
  ),
  /* Multi-shard tile: comment then branch of if-then-else */
  test(
    ~name="Comment then branch of if-then-else",
    ~acts=
      mk({|if true
then 1
else 2¦|})
      @ [Action.Move(Start)]
      @ [Action.Move(Vertical(Down))]
      @ [Action.ToggleLineComment],
    ~goal="if true\n#then 1#¦\nelse 2",
  ),
  /* Multi-shard tile roundtrip */
  test(
    ~name="If-then-else roundtrip then branch",
    ~acts=
      mk({|if true
then 1
else 2¦|})
      @ [Action.Move(Start)]
      @ [Action.Move(Vertical(Down))]
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
    ~goal="if true\nthen 1¦\nelse 2",
  ),
  /* Multi-line with empty line in between */
  test(
    ~name="Comment with empty line between",
    ~acts=
      mk({|x¦|})
      @ string_to_ltr_actions("\n\n")
      @ string_to_ltr_actions("y")
      @ [Action.Select(All)]
      @ [Action.ToggleLineComment],
    ~goal="§?#x#\n##\n# y#¦",
  ),
];

/* Ancestor.sort fix: molds preserved in non-comment contexts */
let ancestor_sort_tests = [
  /* Deleting = from let preserves Pat mold on a */
  remold_test(
    ~name="Molds: delete = from let preserves Pat",
    ~fresh_acts=mk({|let a ¦1 in a|}),
    ~roundtrip_acts=mk({|let a =¦ 1 in a|}) @ [Action.Destruct(Left)],
  ),
  /* type...=...in roundtrip preserves TPat mold */
  remold_test(
    ~name="Molds: type alias roundtrip line 1",
    ~fresh_acts=mk({|type t =
Int
in 1¦|}),
    ~roundtrip_acts=
      mk({|type t =
Int
in 1¦|})
      @ [Action.Move(Start)]
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
  ),
  /* If-then-else roundtrip preserves Exp molds */
  remold_test(
    ~name="Molds: if-then-else roundtrip then line",
    ~fresh_acts=mk({|if true
then 1
else 2¦|}),
    ~roundtrip_acts=
      mk({|if true
then 1
else 2¦|})
      @ [Action.Move(Start)]
      @ [Action.Move(Vertical(Down))]
      @ [Action.ToggleLineComment]
      @ [Action.ToggleLineComment],
  ),
];

/* Test that wrapping a selection across a tile boundary doesn't cause
 * stack overflow. Scenario: (§1)¦ + Insert("(") produces orphan shards
 * from the original (...) tile at different nesting levels. Without the
 * fix, duplicate IDs cause infinite recursion in the elaborator. */
let wrap_calculate_test = [
  test_case(
    "Wrap across tile boundary: no stack overflow in statics",
    `Quick,
    () => {
      let acts =
        mk({|(1)¦|})
        @ [
          Action.Select(Resize(Local(Left, ByChar))),
          Action.Select(Resize(Local(Left, ByChar))),
          Action.Insert("("),
        ];
      let z = perform(Zipper.init(), acts);
      let _statics =
        CachedStatics.init(
          ~settings=Language.CoreSettings.on,
          ~stitch=x => x,
          ~is_dynamic_term=false,
          z,
        );
      check(testable(Fmt.string, String.equal), "ok", "ok", "ok");
    },
  ),
];

/* Cut-paste across delimiter boundaries: when a selection spans a
 * delimiter (e.g. `=` in `let...=...in`), cutting removes the shard
 * from its parent tile. Re-pasting inserts a fresh-ID token that must
 * be reconnected to the ancestor tile via rescan_parent_shards. */
let sel_r_token = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Select(Resize(Local(Right, ByToken))));

let cross_boundary_paste_tests = [
  /* Select `comparison = (0` from `let comparison = (0 == 0) in comparison`,
   * cutting the `=` shard out of the let form and the `(` out of parens.
   * After paste, both should be structurally restored. */
  test_case(
    "Cut-paste spanning = delimiter in let: structural integrity",
    `Quick,
    () => {
      /* Build zipper with caret after `let ` */
      let z =
        mk({|let ¦comparison = (0 == 0) in comparison|})
        |> perform(Zipper.init());
      /* Select right by token: comparison, ws, =, ws, (, 0 */
      let z = perform(z, sel_r_token(6));
      /* Get clipboard text */
      let clipboard =
        Printer.of_segment(
          ~holes=convex_char,
          ~indent="",
          z.selection.content,
        );
      /* Cut then paste */
      let z = perform(z, [Cut, Paste(clipboard)]);
      /* Check structural integrity: no incomplete tiles */
      let seg = Zipper.unselect_and_zip(z);
      let inc = Segment.incomplete_tiles(seg);
      check(
        Alcotest.int,
        "no incomplete tiles (labels: "
        ++ String.concat(
             "; ",
             List.map((t: Tile.t) => String.concat(",", t.label), inc),
           )
        ++ ")",
        0,
        List.length(inc),
      );
    },
  ),
];

let tests = [
  ("Editing.Basic", basic_tests),
  ("Editing.Insertion", insertion_tests),
  ("Editing.Destruction", destruct_tests),
  ("Editing.Move", move_tests),
  ("Editing.Selection", selection_tests),
  ("Editing.Rescan", rescan_tests),
  ("Editing.Paste", paste_tests),
  ("Editing.Module", module_tests),
  ("Editing.ShardTheft", shard_theft_tests),
  ("Editing.SegmentCache", segment_cache_tests),
  ("Editing.RemoldSort", remold_sort_tests),
  ("Editing.WrapSelection", wrap_selection_tests),
  ("Editing.WrapCalculate", wrap_calculate_test),
  ("Editing.UnwrapQuote", unwrap_quote_tests),
  ("Editing.CommentToggle", comment_toggle_tests),
  ("Editing.CommentRemold", comment_remold_tests),
  ("Editing.CommentToggleExtra", comment_toggle_extra_tests),
  ("Editing.AncestorSort", ancestor_sort_tests),
  ("Editing.CrossBoundaryPaste", cross_boundary_paste_tests),
];
