open Alcotest;
open Language;

/* Removing a livelit's projector leaves a well-typed program that means
   the same thing, on every livelit slide.

   A projected use holds its splices as the editor's Splice pieces;
   removing the projector (ProjectorPerform.unsplice_segment) unwraps each
   back into the parens it is written with in text. So the unprojected use
   is exactly the slide's text without `^^livelit(...)`, and statics must
   read those parens as splices where Model says SpliceRef
   (UserLivelit.expose_splice_refs). Each slide is checked projected and
   unprojected: the same errors (none, for all but the error slides), and
   the same value. */

let dir = "hazel-programs/docs/livelits";

let root = () =>
  List.find_opt(Sys.file_exists, [dir, "../../../" ++ dir])
  |> Option.value(~default=dir);

let read = path => {
  let ic = open_in_bin(path);
  let text = really_input_string(ic, in_channel_length(ic));
  close_in(ic);
  text;
};

/* Every .hz under the livelit slides, one folder deep, with a projected
   use in it. */
let slides = () => {
  let r = root();
  let hz = (sub, f) =>
    Filename.check_suffix(f, ".hz") ? [Filename.concat(sub, f)] : [];
  Sys.readdir(r)
  |> Array.to_list
  |> List.sort(compare)
  |> List.concat_map(f => {
       let p = Filename.concat(r, f);
       Sys.is_directory(p)
         ? Sys.readdir(p)
           |> Array.to_list
           |> List.sort(compare)
           |> List.concat_map(hz(f))
         : hz("", f);
     })
  |> List.filter(f => {
       let text = read(Filename.concat(r, f));
       try(Str.search_forward(Str.regexp_string("^^livelit("), text, 0) >= 0) {
       | Not_found => false
       };
     });
};

/* The text with each `^^livelit(e)` replaced by `e`: what removing the
   projector leaves. Parens inside string literals are not counted. */
let unproject = (text: string): string => {
  let tag = "^^livelit(";
  let n = String.length(text)
  and k = String.length(tag);
  let buf = Buffer.create(n);
  /* The index of the paren closing the one opened just before i. */
  let rec close = (i, depth, in_str) =>
    if (i >= n) {
      fail("unbalanced ^^livelit(");
    } else {
      switch (text.[i], in_str) {
      | ('"', _) => close(i + 1, depth, !in_str)
      | (_, true) => close(i + 1, depth, true)
      | ('(', false) => close(i + 1, depth + 1, false)
      | (')', false) when depth == 0 => i
      | (')', false) => close(i + 1, depth - 1, false)
      | _ => close(i + 1, depth, false)
      };
    };
  let rec go = (i, closes) =>
    if (i >= n) {
      ();
    } else if (List.mem(i, closes)) {
      go(i + 1, List.filter(c => c != i, closes));
    } else if (i + k <= n && String.sub(text, i, k) == tag) {
      go(i + k, [close(i + k, 0, false), ...closes]);
    } else {
      Buffer.add_char(buf, text.[i]);
      go(i + 1, closes);
    };
  go(0, []);
  Buffer.contents(buf);
};

let load = (~source, text) =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source, ~root=Exp, text)) {
  | None => fail(source ++ " did not parse")
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  };

let messages = Test_ExpansionErrors.messages;

let check_slide = (file, ()) => {
  let text = read(Filename.concat(root(), file));
  let bare = unproject(text);
  check(
    bool,
    file ++ ": no projector is left",
    false,
    try(Str.search_forward(Str.regexp_string("^^livelit("), bare, 0) >= 0) {
    | Not_found => false
    },
  );
  let (m_p, elab_p) = load(~source=file, text);
  let (m_u, elab_u) = load(~source=file ++ "-unprojected", bare);
  check(
    list(string),
    file ++ ": the same errors unprojected",
    messages(m_p),
    messages(m_u),
  );
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    file ++ ": the same value unprojected",
    Evaluator.evaluate(~env=Builtins.env_init, elab_p) |> fst,
    Evaluator.evaluate(~env=Builtins.env_init, elab_u) |> fst,
  );
};

/* Splices in Text: a use written by hand, unprojected, means what the
   projected one does, and a spliced field written without its parens is
   an error, as the slide says. */
let splices_in_text = () => {
  let text = read(Filename.concat(root(), "splices-in-text.hz"));
  let (m, elab) = load(~source="splices-in-text", text);
  check(list(string), "no errors", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    "means (44, 44)",
    Test_UserLivelits.run("(44, 44)"),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
  /* In the code, not in the slide's comment, which shows the same form. */
  let from = "let by_hand = ^pair((a = (x : Int)";
  let i = Str.search_forward(Str.regexp_string(from), text, 0);
  let bare =
    String.sub(text, 0, i)
    ++ "let by_hand = ^pair((a = x"
    ++ String.sub(
         text,
         i + String.length(from),
         String.length(text) - i - String.length(from),
       );
  check(
    bool,
    "a = x, without parens, is an error",
    true,
    messages(fst(load(~source="bare", bare))) != [],
  );
};

/* Higher-order, Functional Expansion: the slide type-checks, and its use
   is a curried function of a format and an offset, giving text. */
let higher_order_expansion = () => {
  let text = read(Filename.concat(root(), "expansion.hz"));
  let (m, elab) = load(~source="expansion", text);
  check(list(string), "no errors", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    "its value",
    Test_UserLivelits.run(
      "(\"hsl(200 69% 67%)\", \"hsl(200 52% 79%)\", "
      ++ "\"rgb(114 190 228)\", \"hwb(200 20% 10%)\", 4)",
    ),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
};

/* Emotion (Kids' Choice): the slide means how its face reads, and its
   view draws candy only as far as the head is exploded. */
let kids_emotion = () => {
  let text = read(Filename.concat(root(), "emotion-kids.hz"));
  let (m, elab) = load(~source="emotion-kids", text);
  check(list(string), "no errors", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    "means how the face reads",
    Test_UserLivelits.run(
      "(feeling = \"happy\", smile = 85, brow = 30, exploded = 0, candy = 0, "
      ++ "eyes = \"plain\", eye_size = 50, side_lines = 0, rays = 0, teeth = 0, "
      ++ "mouth_open = 0, sickness = 0, unibrow = true)",
    ),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
  /* The view on a model whose cells carry c, b and k. */
  let marker = "\n} in\n\nlet head";
  let cut = Str.search_forward(Str.regexp_string(marker), text, 0) + 5;
  let circles = (~attr="opacity", ~sd=0, ~r=0, c, b, k) => {
    let program =
      String.sub(text, 0, cut)
      ++ " ^kid_face.view((smile = 85, brow = 30, drag = Idle, "
      ++ "color = SpliceRef((\"c\", "
      ++ string_of_int(c)
      ++ ")), burst = SpliceRef((\"b\", "
      ++ string_of_int(b)
      ++ ")), candy = SpliceRef((\"k\", "
      ++ string_of_int(k)
      ++ ")), stars = SpliceRef((\"st\", false)), "
      ++ "hearts = SpliceRef((\"ht\", false)), eyes = SpliceRef((\"e\", 50)), "
      ++ "sides = SpliceRef((\"sd\", "
      ++ string_of_int(sd)
      ++ ")), rays = SpliceRef((\"r\", "
      ++ string_of_int(r)
      ++ ")), "
      ++ "teeth = SpliceRef((\"t\", 0)), opening = SpliceRef((\"o\", 0)), "
      ++ "x_eyes = SpliceRef((\"xe\", false)), sickness = 0, "
      ++ "unibrow = SpliceRef((\"ub\", true))))";
    /* Loaded as the editor loads a slide: Test_UserLivelits.run parses
       another way, which takes ~45 s on a program this size. */
    let (_, elab) = load(~source="emotion-kids-view", program);
    let cmd = Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
    switch (Haz3lcore.ViewCmdRunner.run(cmd)) {
    | Error(e) => fail("view did not run: " ++ e)
    | Ok(h) =>
      check(bool, "Html", true, Haz3lcore.MvuShape.is_html(h));
      /* The pieces of candy and confetti: nodes whose opacity is "1". */
      let visible = attrs =>
        Haz3lcore.MvuShape.(
          switch (of_list(attrs)) {
          | Some(xs) =>
            List.exists(
              a =>
                switch (of_constructor_raw(strip_wrappers(a))) {
                | Some(("Create", nv)) =>
                  switch (of_tuple(strip_wrappers(nv))) {
                  | Some([n, v]) =>
                    of_string(strip_wrappers(n)) == Some(attr)
                    && of_string(strip_wrappers(v)) == Some("1")
                  | _ => false
                  }
                | _ => false
                },
              xs,
            )
          | None => false
          }
        );
      let rec count = d =>
        Haz3lcore.MvuShape.(
          switch (of_constructor_raw(strip_wrappers(d))) {
          | Some(("Node", body)) =>
            switch (of_tuple(strip_wrappers(body))) {
            | Some([_tag, attrs, kids]) =>
              (visible(attrs) ? 1 : 0) + count(kids)
            | _ => 0
            }
          | Some((_, body)) => count(body)
          | None =>
            let d = strip_wrappers(d);
            switch (of_tuple(d), of_list(d)) {
            | (Some(ds), _)
            | (None, Some(ds)) =>
              List.fold_left((n, d) => n + count(d), 0, ds)
            | (None, None) => 0
            };
          }
        );
      count(h);
    };
  };
  check(int, "9 pieces show", 9, circles(90, 20, 30));
  /* Lines are drawn at stroke-opacity 1 when shown, 0 when not. */
  let lines = circles(~attr="stroke-opacity");
  check(int, "no lines at rest", 0, lines(90, 0, 0));
  check(int, "all ten side lines", 10, lines(~sd=100, 90, 0, 0));
  check(int, "all seven rays, head open", 7, lines(~r=100, 90, 100, 0));
  check(int, "no rays while the head is shut", 0, lines(~r=100, 90, 0, 0));
  check(int, "no candy while unexploded", 0, circles(90, 0, 100));
  check(int, "all forty by halfway", 40, circles(90, 50, 50));
};

/* Lockable Cell: unlocked, the use means its code's value. */
let lockable_cell = () => {
  let text = read(Filename.concat(root(), "lockable-cell.hz"));
  let (m, elab) = load(~source="lockable-cell", text);
  check(list(string), "no errors", [], messages(m));
  /* By constructor name and argument: the value's constructor carries
     its sum type, which a literal Unlocked(5) does not. */
  let v = Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
  let reading =
    switch (DHExp.strip_ascriptions(v).term) {
    | Ap(_, {term: Constructor(name, _), _}, arg) =>
      switch (DHExp.strip_ascriptions(arg).term) {
      | Atom(Int(n)) => Some((name, Bigint.to_int_exn(n)))
      | _ => None
      }
    | _ => None
    };
  check(
    option(pair(string, int)),
    "means the code's value, unlocked",
    Some(("Unlocked", 5)),
    reading,
  );
};

/* 1990s Face: the slide means its caption, for the stamp its slider
   picks. */
let nineties_face = () => {
  let text = read(Filename.concat(root(), "nineties-face.hz"));
  let (m, elab) = load(~source="nineties-face", text);
  check(list(string), "no errors", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    "means its caption",
    Test_UserLivelits.run("\"Have a profitable day\""),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
  /* In the corner of fear and horror, the mood joins the caption. */
  let corner =
    text
    |> Str.global_replace(
         Str.regexp_string("^love_dial(50)"),
         "^love_dial(0)",
       )
    |> Str.global_replace(
         Str.regexp_string("^dread_dial(50)"),
         "^dread_dial(100)",
       );
  let (m, elab) = load(~source="nineties-face-corner", corner);
  check(list(string), "no errors in the corner", [], messages(m));
  check(
    Test_Evaluator_Prelude.dhexp_typ,
    "a terrified day",
    Test_UserLivelits.run("\"Have a terrified, profitable day\""),
    Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst,
  );
};

let tests = (
  "Unproject",
  slides()
  |> List.map(f => test_case(f, `Quick, check_slide(f)))
  |> List.append([
       test_case("Splices in Text", `Quick, splices_in_text),
       test_case("Emotion (Kids' Choice)", `Quick, kids_emotion),
       test_case("1990s Face", `Quick, nineties_face),
       test_case("Lockable Cell", `Quick, lockable_cell),
       test_case(
         "Higher-order, Functional Expansion",
         `Quick,
         higher_order_expansion,
       ),
     ]),
);
