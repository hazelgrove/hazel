open Alcotest;
open Language;

/* Livelits / Parameters: a livelit that takes value parameters (Sec.
   2.4.1), used through abbreviations. Each check loads the slide's text
   as the editor loads it, sometimes with one edit, so it tests the slide
   as shipped. */

let file = "parameters.hz";

let read = () => {
  let path =
    List.find_opt(
      Sys.file_exists,
      [
        "hazel-programs/docs/livelits/" ++ file,
        "../../../hazel-programs/docs/livelits/" ++ file,
      ],
    )
    |> Option.value(~default="hazel-programs/docs/livelits/" ++ file);
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
let edited = (~from, ~to_) => {
  let text = read();
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

/* The livelit entry for ^name, from any expression that has it in scope. */
let entry = (m: Statics.Map.t, name) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (acc) {
      | Some(_) => acc
      | None => Ctx.lookup_livelit(Info.ctx_of(info), name)
      },
    m,
    None,
  );

/* What a new use of ^name starts with: its init, performed as the editor
   performs it when you type ^name then a space. */
let init_of = (m, name) =>
  switch (entry(m, name)) {
  | Some({user_def: Some(def), _}) =>
    switch (Haz3lcore.UpdateCmdRunner.init_model(def)) {
    | Ok(model) => model
    | Error(e) => fail("init of ^" ++ name ++ " failed: " ++ e)
    }
  | _ => fail("^" ++ name ++ " is not a user livelit here")
  };

let has = (msg, m) => List.mem(msg, messages(m));

let tests = (
  "Parameters",
  [
    test_case(
      "The slide means 25 + 3",
      `Quick,
      () => {
        let (m, elab) = load(~source="parameters", read());
        check(list(string), "no errors", [], messages(m));
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          file,
          Test_UserLivelits.run("28"),
          Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
        );
      },
    ),
    /* init reads the parameters: a new use starts mid-range. */
    test_case(
      "A new ^percent starts at 50, a new ^die at 3",
      `Quick,
      () => {
        let (m, _) = load(~source="parameters", read());
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          "^percent",
          Test_UserLivelits.run("50"),
          init_of(m, "percent"),
        );
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          "^die",
          Test_UserLivelits.run("3"),
          init_of(m, "die"),
        );
      },
    ),
    test_case("^slider used directly is an error", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        has(
          "^slider takes parameters: give them with let ^name = ^slider(args) in, then use ^name",
          fst(
            load(
              ~source="direct",
              edited(~from="^^livelit(^die(3))", ~to_="^slider(3)"),
            ),
          ),
        ),
      )
    ),
    /* Parameters are closed: a client binding is not in scope there. */
    test_case("An argument naming a client binding is an error", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        has(
          "Variable six is not bound",
          fst(
            load(
              ~source="open",
              edited(
                ~from="let ^die = ^slider(1, 6) in",
                ~to_="let six = 6 in let ^die = ^slider(1, six) in",
              ),
            ),
          ),
        ),
      )
    ),
    test_case("An argument of the wrong type is an error", `Quick, () =>
      check(
        bool,
        "marked",
        true,
        has(
          "Expecting type Int but got inconsistent type String",
          fst(
            load(
              ~source="mistyped",
              edited(~from="^slider(1, 6)", ~to_="^slider(\"one\", 6)"),
            ),
          ),
        ),
      )
    ),
  ],
);
