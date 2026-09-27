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

let tests = (
  "Unproject",
  slides() |> List.map(f => test_case(f, `Quick, check_slide(f))),
);
