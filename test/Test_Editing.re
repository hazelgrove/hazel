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

let perform = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  /* Compute statics so that Smart selection can look up parent terms */
  let statics_settings = {
    ...Language.CoreSettings.off,
    statics: true,
  };
  let perform = (a: Action.t, z: Zipper.t) => {
    let term = MakeTerm.from_zip_for_sem(z).term;
    let statics =
      CachedStatics.init_from_term(
        ~settings=statics_settings,
        ~is_dynamic_term=true,
        term,
      );
    Perform.go(
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
  /* This harness uses a  to represent caret position.
   * This assumes there are no literal instances of the caret
   * char proceeding the caret ¦ in the syntax. This creates
   * a list of actions intended to insert the init string into the
   * zipper character-by-character, except for the caret character,
   * and then move left character by character until the indicated
   * caret position is reached */
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
  let (before, after) = split([], Token.to_list(init));
  let init_without_caret_clusters = before @ after;
  let init_without_caret = Token.of_list(init_without_caret_clusters);
  /* After inserting all characters, we need to move left by the number
   * of characters that come after the caret position */
  let chars_after_caret = List.length(after);
  string_to_ltr_actions(init_without_caret) @ mv_l(chars_after_caret);
};

let test = (~name, ~acts, ~goal): test_case(_) =>
  test_case(name, `Quick, () =>
    check(
      testable(Fmt.string, String.equal),
      goal,
      goal,
      acts |> perform(Zipper.init()) |> printer,
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
    ~acts=mk("(¦)") @ [Paste(String({|"foo"|}))],
    ~goal={|("foo"¦)|},
  ),
  test(
    ~name="Paste string splitting token",
    ~acts=mk("1¦1") @ [Paste(String({|"foo"|}))],
    ~goal={|1~"foo"¦~1|},
  ),
  test(
    ~name="Paste string splitting consecutive delimiters",
    ~acts=mk("if¦then") @ [Paste(String({|"foo"|}))],
    ~goal={|if"foo"¦then?|},
  ),
  test(
    ~name="Paste string with a backpack glom false friend",
    ~acts=mk("¦") @ [Paste(String({|([)(|}))],
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
    ~acts=mk({|"¦"|}) @ [Paste(String("😄"))],
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
    ~goal={|x¦if~ 1 then 2 else 3|},
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
      mk({|1¦+2|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
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
      mk({|¦f(x)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
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
      mk({|f(¦x)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
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
      mk({|f¦(x)|})
      @ [Action.Select(Smart(2)), Action.Select(Smart(3))],
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
      mk({|1 + ¦2|})
      @ [Select(Term(Current)), Select(Term(Current))],
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
    ~acts=
      mk({|1 ¦+ 2|})
      @ [Select(Smart(2)), Select(Term(Current))],
    ~goal={|§1 + 2¦|},
  ),
  test(
    ~name="Cmd+D on function name: select fn, then escalate to app",
    ~acts=
      mk({|¦f(x)|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§f(x)¦|},
  ),
  test(
    ~name="Cmd+D on arg inside app: select arg, then app",
    ~acts=
      mk({|f(¦x)|})
      @ [Select(Term(Current)), Select(Term(Current))],
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
    ~acts=
      mk({|let x = 1 in let y = 2 in ¦y|})
      @ [Select(Term(Current))],
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
      @ [
        Select(Term(Current)),
        Select(Term(Current)),
      ],
    ~goal={|§let x = 1 in x¦|},
  ),
  test(
    ~name="Cmd+D inside tuple element: select element, then parens",
    ~acts=
      mk({|(1, ¦2, 3)|})
      @ [Select(Term(Current)), Select(Term(Current))],
    ~goal={|§(1, 2, 3)¦|},
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
];

let tests = [
  ("Editing.Basic", basic_tests),
  ("Editing.Insertion", insertion_tests),
  ("Editing.Destruction", destruct_tests),
  ("Editing.Move", move_tests),
  ("Editing.Selection", selection_tests),
  ("Editing.Module", module_tests),
];
