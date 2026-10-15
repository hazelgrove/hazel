open Alcotest;
open Language;

/* Livelits / Result View: a sheet whose cells show their results with
   result_view, and whose formula bar is an editor (Sec. 3.2.3, "Result
   Rendering"). The checks load the slide's text as shipped. */

let file = "result-view.hz";

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

/* How many nodes of Html constructor `name` the tree holds. */
let rec count = (name, d) =>
  Haz3lcore.MvuShape.(
    switch (of_constructor_raw(d)) {
    | Some((n, body)) => (n == name ? 1 : 0) + count(name, body)
    | None =>
      switch (of_tuple(d), of_list(d)) {
      | (Some(ds), _)
      | (None, Some(ds)) =>
        List.fold_left((k, d) => k + count(name, d), 0, ds)
      | (None, None) => 0
      }
    }
  );

let tests = (
  "ResultView",
  [
    test_case(
      "The slide means (12, 3, 36)",
      `Quick,
      () => {
        let (m, elab) = load(~source="result-view", read());
        check(list(string), "no errors", [], messages(m));
        check(
          Test_Evaluator_Prelude.dhexp_typ,
          file,
          Test_UserLivelits.run("(12, 3, 36)"),
          Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
        );
      },
    ),
    /* The view, on refs carrying 12, a hole and 36: two results drawn,
       and the hole's cell shows the view's own dash. */
    test_case(
      "The view draws each value, and a dash for a hole",
      `Quick,
      () => {
        let slide = read();
        let marker = "\n} in";
        let rec find = i =>
          if (i + String.length(marker) > String.length(slide)) {
            fail("no end of the ^sheet definition");
          } else if (String.sub(slide, i, String.length(marker)) == marker) {
            i + String.length(marker);
          } else {
            find(i + 1);
          };
        let program =
          String.sub(slide, 0, find(0))
          ++ " ^sheet.view((sel = 0, a = SpliceRef((\"a\", 12)), "
          ++ "b = SpliceRef((\"b\", ?)), c = SpliceRef((\"c\", 36))))";
        switch (Haz3lcore.ViewCmdRunner.run(Test_UserLivelits.run(program))) {
        | Error(e) => fail("view did not run: " ++ e)
        | Ok(h) =>
          check(bool, "Html", true, Haz3lcore.MvuShape.is_html(h));
          check(int, "two results", 2, count("SpliceResult", h));
          check(int, "one editor", 1, count("Splice", h));
        };
      },
    ),
  ],
);
