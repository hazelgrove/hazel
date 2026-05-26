// Slide.re: address documentation slides by name and serialize back to .ml.
//
// Round-trip logic (text rendering, parsing back, implicit-hole stripping)
// lives in `Haz3lcore.TextRoundtrip`. This module is the thin CLI-facing
// shim that resolves slides by name and emits the `let out : ... PersistentSegment.t`
// module form the slide source files use.

open Haz3lcore;

module Buffer = Stdlib.Buffer;
module Printf = Stdlib.Printf;
module String = Stdlib.String;

type slide = {
  title: string,
  persisted: PersistentSegment.t,
};

let all_slides: list((string, PersistentSegment.t)) = Web.Init.documentation_slides;

let list_names: list(string) = all_slides |> List.map(fst);

let find = (name: string): option(slide) =>
  all_slides
  |> List.find_opt(((n, _)) => n == name)
  |> Option.map(((title, persisted)) =>
       {
         title,
         persisted,
       }
     );

let slide_to_text =
    (~implicit_hole=TextRoundtrip.default_implicit_hole, slide: slide): string =>
  TextRoundtrip.to_text(~implicit_hole, slide.persisted);

let escape_for_ocaml = (s: string): string => {
  let buf = Buffer.create(String.length(s) + 16);
  String.iter(
    fun
    | '\n' => Buffer.add_string(buf, "\\n")
    | '\r' => Buffer.add_string(buf, "\\r")
    | '\t' => Buffer.add_string(buf, "\\t")
    | '"' => Buffer.add_string(buf, "\\\"")
    | '\\' => Buffer.add_string(buf, "\\\\")
    | c when Char.code(c) < 32 || Char.code(c) >= 127 =>
      Buffer.add_string(buf, Printf.sprintf("\\%03d", Char.code(c)))
    | c => Buffer.add_char(buf, c),
    s,
  );
  Buffer.contents(buf);
};

let render_slide_file = (slide: slide): string => {
  let p = slide.persisted;
  Printf.sprintf(
    "let out : string * Haz3lcore.PersistentSegment.t =\n  ( \"%s\",\n    {\n      segment = \"%s\";\n      backup_text = \"%s\";\n      refractors = \"%s\";\n    } )\n",
    escape_for_ocaml(slide.title),
    escape_for_ocaml(p.segment),
    escape_for_ocaml(p.backup_text),
    escape_for_ocaml(p.refractors),
  );
};

let text_to_slide =
    (
      ~implicit_hole=TextRoundtrip.default_implicit_hole,
      ~title: string,
      text: string,
    )
    : slide =>
  switch (TextRoundtrip.persist_from_text(~implicit_hole, ~root=Exp, text)) {
  | None => failwith("slide: failed to parse program text")
  | Some(persisted) => {
      title,
      persisted,
    }
  };
