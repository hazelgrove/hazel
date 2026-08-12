open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

/* Lossless: holes print as the ¿ marker (MarkerParse destructs them on
   read), so crash-recovery text and share links keep hole positions. */
let to_string =
  Printer.of_zipper(
    ~holes=Token.implicit_hole_marker,
    ~concave_holes=Token.implicit_hole_marker,
    ~indent="",
  );

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

/* Persisted programs are complete, so the linear Menhir zip usually
   takes this; the simulated-typing parser is quadratic in program
   size and can hang startup on big stale-sexp slides. */
/* Caret starts at the TOP: unzip's default direction (Right) leaves the
   caret after the whole program, and the editor scrolls the caret into
   view on display — a freshly loaded slide would open at the bottom. */
let apply_collected_refractors = (z: Zipper.t): Zipper.t =>
  List.fold_left(
    (z, (id, trigger)) =>
      switch (Triggers.refractor_of_invoke_token(trigger)) {
      | Some((kind, model)) => ZipperBase.add_manual(~model?, id, kind, z)
      | None => z
      },
    z,
    FastParse.collected_refractors^,
  );

let from_backup_text = (backup_text: string, ~root): Zipper.t => {
  /* Strip only the writer's final newline (see persist); all other edge
     whitespace is content — leading/trailing blank lines round-trip. */
  let text = StringUtil.strip_final_newline(backup_text);
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root,
      text,
    )
  ) {
  | Some(segment) =>
    Zipper.unzip(~direction=Left, segment) |> apply_collected_refractors
  | None =>
    /* MarkerParse subsumes the plain typing parse and also destructs
       `¿` markers back into Grout (concave grout and other fast-path
       bails land here). Console-visible: every slow parse names itself. */
    print_endline(
      "SLOW PARSE (persistence load, "
      ++ string_of_int(String.length(text))
      ++ " chars): "
      ++ Option.value(FastParse.bail_note^, ~default="no note")
      ++ " | head: "
      ++ String.sub(text, 0, min(60, String.length(text))),
    );
    switch (MarkerParse.of_text(~root, text)) {
    | None => Zipper.init()
    | Some(z) =>
      /* reposition the caret to the start WITHOUT dropping refractors:
         unselect_and_zip yields a bare segment, and unzip would mint a
         fresh (empty) refractor state — losing pins built from trigger
         text during the parse */
      let refractors = z.refractors;
      Zipper.unzip(~direction=Left, Zipper.unselect_and_zip(z))
      |> ZipperBase.update_refractors(_, _ => refractors);
    };
  };
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
