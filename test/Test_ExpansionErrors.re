open Alcotest;
open Language;

/* Expansion type errors, one kind of expand at a time.

   A Functional expand is an ordinary function, Model -> Expansion,
   checked where it is written: its errors are ordinary type errors in
   the definition, and no use is marked.

   A Macro expand returns code, and a use means that code applied to the
   use's splices; the code must take each splice, at its code's type, to
   Expansion (Fig. 5 premise 5), which only a use can check. Its error is
   on the use, and says what failed in terms the definition states --
   Expansion, the splices, the code -- not the arrow type the check is
   phrased with, which appears nowhere in the program.

   The slides in Livelits / Expansion Type Errors are the fixtures, loaded
   as the editor loads them, so each checks that its slide shows what its
   comments say. */

/* Every error message statics reports, as the editor prints them. */
let messages = (m: Statics.Map.t): list(string) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (Info.marks_of(info)) {
      | [] => acc
      | marks => [Haz3lcore.ErrorPrint.string_of_marks(info, marks), ...acc]
      },
    m,
    [],
  )
  |> List.sort_uniq(String.compare);

/* How many uses carry a use-site expansion mark. A use spreads one info
   over several ids, and the editor's ^^livelit(...) wrapper carries the
   mark too, so count the livelit applications, ^name(...), themselves. */
let marked_uses = (m: Statics.Map.t): int =>
  Id.Map.fold(
    (_, info, acc) =>
      switch ((info: Info.t)) {
      | InfoExp({marks, user_term, _})
          when
            (
              switch (user_term.term) {
              | Ap(_, {term: LivelitName(_), _}, _) => true
              | _ => false
              }
            )
            && List.exists(
                 fun
                 | Mark.BadMacroExpansion(_)
                 | Mark.BadLivelitExpansion(_) => true
                 | _ => false,
                 marks,
               ) =>
        Id.Set.add(Exp.rep_id(user_term), acc)
      | _ => acc
      },
    m,
    Id.Set.empty,
  )
  |> Id.Set.cardinal;

let slide = file => fst(Test_Quote.slide("expansion-errors/" ++ file));

/* The slide reports exactly these errors, and marks this many uses. */
let shows = (file, ~uses, expected) =>
  test_case(
    file,
    `Quick,
    () => {
      let m = slide(file);
      check(list(string), file ++ ": its errors", expected, messages(m));
      check(int, file ++ ": uses marked", uses, marked_uses(m));
    },
  );

/* Test_UserLivelits.def with a Macro expand over no splices. */
let macro_def = (~expansion, ~code) =>
  "{
type Model = Int;
type Action = Int;
type Expansion = "
  ++ expansion
  ++ ";
let init = Pure(0);
let update = fun m -> fun a -> Pure(a);
let view = fun m -> Pure(Html.text(\"\"));
let expand = Macro(fun m -> (quote "
  ++ code
  ++ " end, []))
}";

let inline = (~expansion, ~code) =>
  fst(
    Test_UserLivelits.statics(
      "let ^s = " ++ macro_def(~expansion, ~code) ++ " in ^s(1)",
    ),
  );

let tests = (
  "ExpansionErrors",
  [
    shows("errors-about.hz", ~uses=0, []),
    /* Functional: in the definition, as ordinary errors. */
    shows(
      "functional-result.hz",
      ~uses=0,
      ["Expecting type String but got inconsistent type Int"],
    ),
    /* m's type prints as the alias it was annotated with. */
    shows(
      "functional-body.hz",
      ~uses=0,
      ["Expecting type String but got inconsistent type Model"],
    ),
    shows(
      "functional-model.hz",
      ~uses=0,
      ["Expecting type Int but got inconsistent type String"],
    ),
    /* Macro: on the use, in terms of Expansion and the splices. */
    shows(
      "macro-result.hz",
      ~uses=1,
      [
        "Applied to its splice, the expansion's code has type Int, but the livelit declares Expansion = String",
      ],
    ),
    /* Two uses, and only the one whose splice holds an Int is marked. */
    shows(
      "macro-parameter.hz",
      ~uses=1,
      [
        "The expansion's code takes String for splice 1, whose code has type Int",
      ],
    ),
    shows(
      "macro-not-a-function.hz",
      ~uses=1,
      [
        "The expansion's code has type Int, but must be a function of its splice, to Expansion = Int",
      ],
    ),
    /* With no splices the code itself must have type Expansion. */
    test_case("Macro, no splices: the code is not Expansion", `Quick, () =>
      check(
        list(string),
        "errors",
        [
          "The expansion's code has type Int, but the livelit declares Expansion = String",
        ],
        messages(inline(~expansion="String", ~code="1")),
      )
    ),
    /* An error inside the quotation is marked where it is written, and
       the use says the code has an error, not that Int is not Int. */
    test_case("Macro: an error inside the quotation", `Quick, () =>
      check(
        list(string),
        "errors",
        [
          "Expecting type Int but got inconsistent type String",
          "The expansion's code has a type error",
        ],
        messages(inline(~expansion="Int", ~code="1 + \"one\"")),
      )
    ),
    /* The control: a Macro whose code fits reports nothing. */
    test_case("Macro: code that fits is unmarked", `Quick, () =>
      check(
        list(string),
        "errors",
        [],
        messages(inline(~expansion="Int", ~code="1")),
      )
    ),
  ],
);
