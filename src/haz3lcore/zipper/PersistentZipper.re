open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

/* Lossless: holes print as the ¿ marker (MarkerParse destructs them on
   read), so crash-recovery text and share links keep hole positions. */
let to_string = z => MarkerParse.to_text(z);

/* Stored text = printed content + one final newline; readers strip
   exactly that one (strip_final_newline in from_backup_text). Matches
   the POSIX final newline in committed .hz files, and keeps buffers
   that genuinely end in blank lines lossless across a round-trip. */
let persist = (zipper: Zipper.t) => {
  {
    zipper: Zipper.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(zipper) ++ "\n",
  };
};

/* A text-backed slide: the program text (a committed .hz file) is the
   only source. The empty zipper string marks the text path as
   intentional, so unpersist takes it without the stale-serialization
   warning. */
let of_text = (text: string): t => {
  zipper: "",
  backup_text: text,
};

/* Slide-source ingestion: committed .hz text keeps human indentation,
   but Hazel computes indentation at layout time and renders literal
   leading spaces ON TOP of it (doubled, drifting) — so slide text is
   flattened here. The strip is blind per-line; slide sources must not
   contain multi-line string literals. */
let of_slide_text = (text: string): t =>
  of_text(StringUtil.trim_leading(text));

/* Fast-first text→zipper, shared by persistence load and the CLI:
   FastParse (linear, complete terms) with pin collection, then the
   ¿-aware recovering parser. Persisted programs are complete, so the
   linear zip usually takes it — the simulated-typing parser is quadratic
   in program size and can hang startup on big stale-sexp slides. Returns
   None only when both parsers fail; the failure POLICY lives at the call
   sites (the CLI reports an error, persistence loads an empty buffer).
   Only the writer's final newline is stripped (see persist); all other
   edge whitespace is content — leading/trailing blank lines round-trip. */
let parse_text = (~source: string, ~root, text: string): option(Zipper.t) => {
  let text = StringUtil.strip_final_newline(text);
  switch (
    FastParse.parsed_of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root,
      text,
    )
  ) {
  | Ok({segment, refractors}) =>
    /* Caret starts at the TOP: unzip's default direction (Right) leaves
       the caret after the whole program, and the editor scrolls the caret
       into view on display — a freshly loaded slide would open at the
       bottom.

       Regrout as edits do: the append-a-hole retry strips the trailing
       hole, and printing a hole-less fragment drops its refractors
       (Segment.skel fails). First save adds the final marker. */
    Some(
      Zipper.unzip(~direction=Left, segment)
      |> Zipper.remold_regrout(Left, ~root)
      |> Triggers.apply_refractors(refractors),
    )
  | Error(why) =>
    /* MarkerParse subsumes the plain typing parse and also destructs
       `¿` markers back into Grout (concave grout and other fast-path
       bails land here). Console-visible: every slow parse names itself. */
    print_endline(
      "SLOW PARSE ("
      ++ source
      ++ ", "
      ++ string_of_int(String.length(text))
      ++ " chars): "
      ++ why
      ++ " | head: "
      ++ String.sub(text, 0, min(60, String.length(text))),
    );
    switch (MarkerParse.of_text(~root, text)) {
    | None => None
    | Some(z) =>
      /* reposition the caret to the start WITHOUT dropping refractors:
         unselect_and_zip yields a bare segment, and unzip would mint a
         fresh (empty) refractor state — losing pins built from trigger
         text during the parse */
      let refractors = z.refractors;
      Some(
        Zipper.unzip(~direction=Left, Zipper.unselect_and_zip(z))
        |> ZipperBase.update_refractors(_, _ => refractors),
      );
    };
  };
};

/* Persistence never hard-fails: boot has no error channel, and one
   unreadable blob must not brick the app — load an empty buffer
   instead. Should be near-unreachable (MarkerParse is recovering by
   construction), so it announces itself when it does fire. */
let from_backup_text = (backup_text: string, ~root): Zipper.t =>
  switch (parse_text(~source="persistence load", ~root, backup_text)) {
  | Some(z) => z
  | None =>
    print_endline("PARSE FAILED (persistence load): loading empty buffer");
    Zipper.init();
  };

let unpersist = (persisted: t, ~root) =>
  if (persisted.zipper == "") {
    from_backup_text(persisted.backup_text, ~root);
  } else {
    try(Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp) {
    | _ =>
      print_endline(
        "Warning: using backup text! Serialization may be for an older version of Hazel.",
      );
      from_backup_text(persisted.backup_text, ~root);
    };
  };
