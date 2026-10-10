open Alcotest;
open Language;

/* Livelits / Either, Two Versions: one livelit whose expansion type
   varies by use, written with Expansion = ? and with a type parameter.
   Each check loads a slide's text as the editor loads it, sometimes
   with one edit, so it tests the slide as shipped. */

let read = file => {
  let path =
    List.find_opt(
      Sys.file_exists,
      [
        "hazel-programs/docs/livelits/either/" ++ file,
        "../../../hazel-programs/docs/livelits/either/" ++ file,
      ],
    )
    |> Option.value(~default="hazel-programs/docs/livelits/either/" ++ file);
  let ic = open_in_bin(path);
  let text = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  text;
};

let load = (~source, text) =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source, ~root=Exp, text)) {
  | None => fail(source ++ " did not parse")
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  };

/* The slide's text with one piece replaced; fails if it is not there. */
let edited = (file, ~from, ~to_) => {
  let text = read(file);
  let n = String.length(text)
  and k = String.length(from);
  let rec find = i =>
    if (i + k > n) {
      fail(file ++ ": no " ++ from);
    } else if (String.sub(text, i, k) == from) {
      i;
    } else {
      find(i + 1);
    };
  let i = find(0);
  String.sub(text, 0, i) ++ to_ ++ String.sub(text, i + k, n - i - k);
};

let messages = Test_ExpansionErrors.messages;

let means = (file, (m, elab)) => {
  check(list(string), file ++ ": no errors", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    file,
    Test_UserLivelits.run("(2, \"goodbye!\")"),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
};

let tests = (
  "Either",
  [
    test_case("About", `Quick, () =>
      check(
        list(string),
        "no errors",
        [],
        messages(fst(load(~source="about", read("either-about.hz")))),
      )
    ),
    test_case("1. Unknown Expansion means (2, \"goodbye!\")", `Quick, () =>
      means(
        "either-unknown.hz",
        load(~source="unknown", read("either-unknown.hz")),
      )
    ),
    test_case("2. Type Parameter means (2, \"goodbye!\")", `Quick, () =>
      means(
        "either-typed.hz",
        load(~source="typed", read("either-typed.hz")),
      )
    ),
    /* With ?, a client's misuse of the result is not a static error... */
    test_case("1: n ++ \"!\" is not caught statically", `Quick, () =>
      check(
        list(string),
        "no errors",
        [],
        messages(
          fst(
            load(
              ~source="unknown-misuse",
              edited(
                "either-unknown.hz",
                ~from="(n + 1, s ++ \"!\")",
                ~to_="(n ++ \"!\", s ++ \"!\")",
              ),
            ),
          ),
        ),
      )
    ),
    /* ...with a type parameter, it is. */
    test_case("2: n ++ \"!\" is a static error", `Quick, () =>
      check(
        list(string),
        "the misuse",
        ["Expecting type String but got inconsistent type Int"],
        messages(
          fst(
            load(
              ~source="typed-misuse",
              edited(
                "either-typed.hz",
                ~from="(n + 1, s ++ \"!\")",
                ~to_="(n ++ \"!\", s ++ \"!\")",
              ),
            ),
          ),
        ),
      )
    ),
    /* A splice of the wrong type is an expansion error at the use. */
    test_case("2: a String case of ^either_int", `Quick, () =>
      check(
        list(string),
        "the use",
        [
          /* The code, fun a -> fun _ -> a, is unannotated: it synthesizes
             ? -> ? -> ?, so the mismatch shows only when it is analyzed
             against String -> Int -> Int, as an error in the code. */
          "The expansion's code has a type error, given its 2 splices",
        ],
        messages(
          fst(
            load(
              ~source="typed-splice",
              edited("either-typed.hz", ~from="a=(1)", ~to_="a=(\"one\")"),
            ),
          ),
        ),
      )
    ),
    /* The definition itself cannot be used: it takes a type argument. */
    test_case("2: ^either used without its type argument", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        List.mem(
          "^either takes a type argument: give it one with let ^name = ^either@<Type> in",
          messages(
            fst(
              load(
                ~source="typed-direct",
                edited(
                  "either-typed.hz",
                  ~from="^either_int((a_label",
                  ~to_="^either((a_label",
                ),
              ),
            ),
          ),
        ),
      )
    ),
  ],
);
