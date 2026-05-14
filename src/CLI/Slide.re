// Slide.re: address documentation slides by name and serialize back to .ml.
//
// All documentation slides are already linked into the binary via
// `Web.Init.documentation_slides : list((string, PersistentSegment.t))`. The
// CLI looks them up by name rather than re-parsing the source .ml files.
// Encoding round-trips through PersistentSegment.persist + a hand-rolled
// emitter for the `let out : ... PersistentSegment.t = (...)` module form
// the slide source files use.

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

// Render the slide's program as plaintext, emitting `^^probe(...)` /
// `^^statics(...)` trigger syntax for manual refractors so the text is a
// faithful, reparseable representation of the slide.
let slide_to_text = (slide: slide): string => {
  let z = PersistentSegment.restore(slide.persisted);
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, z);
  Printer.of_segment(
    ~holes="?",
    ~indent=" ",
    ~refractors=z.refractors.manuals,
    segment,
  );
};

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

// Build a slide from plaintext. Refractors carried in the text via
// `^^probe(...)` / `^^statics(...)` trigger syntax are reconstructed by the
// parser's Triggers module on insertion and end up on the resulting zipper.
let text_to_slide = (~title: string, text: string): slide =>
  switch (Parser.to_zipper(~root=Exp, text)) {
  | None => failwith("slide: failed to parse program text")
  | Some(z) => {
      title,
      persisted: PersistentSegment.persist(z),
    }
  };
